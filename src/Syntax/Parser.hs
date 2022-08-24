module Syntax.Parser
  ( parse,
    parseExpr,
  )
where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State.Lazy
import Data.Bifunctor qualified as Bifunctor
import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe
  ( isJust,
    isNothing,
  )
import Data.Text qualified as T
import Syntax.Absyn qualified as Absyn
import Syntax.Interner qualified as Interner
import Syntax.Lexer qualified as Lexer
import Syntax.Parser.Session

parse :: String -> [Lexer.Token] -> Either [SyntaxError] Absyn.Program
parse filename tokens = Bifunctor.first (\_ -> syntaxErrors session') parseRes
  where
    session = mkSession filename tokens
    action = runExceptT $ expect parseProgram "Expected top level definition"
    (parseRes, session') = runState action session

parseProgram :: ParseRes Absyn.Program
parseProgram = Absyn.Program <$> parseDeclarations []

parseDeclarations :: [Absyn.Decl] -> ParseRes [Absyn.Decl]
parseDeclarations decls = do
  curr <- currToken
  if curr == Lexer.EOF
    then return $ reverse decls
    else do
      decl <- expect parseDecl "expected declaration on the top level"
      parseDeclarations (decl : decls)
  where
    parseDecl :: ParseRes Absyn.Decl
    parseDecl = parseFunDecl `choice` parseVarDecl `choice` parseTypeDecl

    parseTypeDecl = throwError NotThisFn
    parseFunDecl = throwError NotThisFn

    parseVarDecl :: ParseRes Absyn.Decl
    parseVarDecl = do
      check Lexer.Var
      id <- parseIdentifier `expect` "expected a name after a var keyword"
      Lexer.Assignment `matchExpect` "Expected an = sign after a variable name"
      init <- parseExpr `expect` "expected init expression"
      return $
        Absyn.VariableDecl
          { name = id,
            varType = Nothing,
            initExpr = init
          }

parseExpr :: ParseRes Absyn.Expr
parseExpr = parseAddition
  where
    parseSequence :: ParseRes Absyn.Expr
    parseSequence = do
      left <- parseExpr'
      go left
      where
        go left = do
          s <- match Lexer.Semicolon
          if isJust s
            then do
              right <- parseExpr' `expect` "Expected expression after ;"
              go $ Absyn.Sequence left right
            else return left

    parseExpr' = parseStmt `choice` parseAssignment

    parseStmt :: ParseRes Absyn.Expr
    parseStmt = go parseFns
      where
        parseFns =
          [ (Lexer.If, parseIf),
            (Lexer.While, parseWhile),
            (Lexer.Break, return Absyn.Break),
            (Lexer.For, parseFor),
            (Lexer.Let, parseLet)
          ]
        go [] = throwError NotThisFn
        go ((tok, f) : fs) =
          match tok >>= \case
            Just _ -> f
            Nothing -> go fs

    parseLet :: ParseRes Absyn.Expr
    parseLet = do
      decls <- parseDeclarations [] `expect` "expected declarations in let"
      Lexer.In `matchOrErr` "expected 'in' keyword"
      body <- parseExpr `expect` "expected body for let"
      Lexer.End `matchOrErr` "expected 'end' keyword"
      return $ Absyn.Let decls body

    parseIf :: ParseRes Absyn.Expr
    parseIf = do
      cond <- parseAddition `expect` "expected if condition"
      Lexer.Then `matchOrErr` "expected 'then' keyword after condition"
      body <- parseExpr `expect` "expected if's body"
      match Lexer.Else >>= \case
        Nothing -> return $ Absyn.If cond body Absyn.Nil
        Just _ -> do
          elseBody <- parseSequence `expect` "expected else's body"
          return $ Absyn.If cond body elseBody

    parseWhile :: ParseRes Absyn.Expr
    parseWhile = do
      cond <- parseAddition `expect` "expected while's condition"
      Lexer.Then `matchOrErr` "expected 'then' keyword after condition"
      body <- parseExpr `expect` "expected while's body"
      return $ Absyn.While cond body

    parseFor :: ParseRes Absyn.Expr
    parseFor = do
      id <- parseIdentifier `expect` "expected var declaration in for"
      Lexer.Assignment `matchOrErr` "expected = in for"
      init <- parseExpr `expect` "expected init expression for for variable"
      Lexer.To `matchOrErr` "expdcted 'to' keyword"
      limit <- parseExpr `expect` "expected expression as for limit"
      Lexer.Do `matchOrErr` "expected a 'do' keyword"
      body <- parseExpr `expect` "expected for's body"
      return $
        Absyn.For
          { forVar = id,
            forVarInit = init,
            forLimit = limit,
            forBody = body
          }

    parseAssignment :: ParseRes Absyn.Expr
    parseAssignment = do
      left <- parseEq
      assign <- match Lexer.Assignment
      if isJust assign
        then do
          right <- parseEq `expect` "Expected expression after assignment"
          return $ Absyn.Assignment left right
        else return left

    parseEq =
      parseBinaryExpr
        [ (Lexer.Equal, "==", Absyn.Equal),
          (Lexer.NotEqual, "!=", Absyn.NotEqual),
          (Lexer.GreaterThan, ">", Absyn.Gt),
          (Lexer.LessThan, "<", Absyn.Lt),
          (Lexer.GreaterOrEq, ">=", Absyn.GtEq),
          (Lexer.LessOrEq, "<=", Absyn.LtEq)
        ]
        parseAnd

    parseAnd =
      parseBinaryExpr
        [(Lexer.And, "&&", Absyn.And)]
        parseOr

    parseOr = parseBinaryExpr [(Lexer.Or, "||", Absyn.Or)] parseBoolNeg

    parseBoolNeg :: ParseRes Absyn.Expr
    parseBoolNeg = do
      t <- currToken
      case t of
        Lexer.Negation -> do
          eat
          e <- parseAddition `expect` "Expected expression after negation"
          return $ Absyn.BoolNegate e
        _ -> parseAddition

    parseAddition =
      parseBinaryExpr
        [ (Lexer.Plus, "+", Absyn.Add),
          (Lexer.Minus, "-", Absyn.Sub)
        ]
        parseMultiplication

    parseMultiplication =
      parseBinaryExpr
        [ (Lexer.Multiplication, "*", Absyn.Mult),
          (Lexer.Divide, "/", Absyn.Division)
        ]
        parseUnaryExpr

    parseBinaryExpr ::
      {- options -}
      [(Lexer.Token, String, Absyn.Expr -> Absyn.Expr -> Absyn.Expr)] ->
      {- nextParseFn -}
      ParseRes Absyn.Expr ->
      {- state -}
      ParseRes Absyn.Expr
    parseBinaryExpr options next = do
      left <- next
      go left options next
      where
        go left [] _ = return left
        go left ((t, name, cons) : tt) next = do
          isToken <- match t
          if isJust isToken
            then do
              right <- next `expect` ("expected expression after " ++ name)
              go (cons left right) options next
            else go left tt next

    parseUnaryExpr :: ParseRes Absyn.Expr
    parseUnaryExpr = do
      t <- currToken
      case t of
        Lexer.Minus -> do
          eat
          e <- parseUnaryExpr `expect` "Expected expression after unary -"
          return $ Absyn.Negate e
        _ -> parseAccessOrIndex

    parseAccessOrIndex :: ParseRes Absyn.Expr
    parseAccessOrIndex = do
      left <- parsePrimaryExpr
      goAccess left
      where
        goAccess :: Absyn.Expr -> ParseRes Absyn.Expr
        goAccess l = do
          dot <- match Lexer.Dot
          case dot of
            Nothing -> goIndex l
            Just _ -> do
              field <- parseIdentifier `expect` "Expected an dentifier after access expressio"
              goAccess $ Absyn.Access l field
        goIndex :: Absyn.Expr -> ParseRes Absyn.Expr
        goIndex l = do
          lparen <- match Lexer.LSquareBracket
          case lparen of
            Nothing -> return l
            Just _ -> do
              inner <- parseExpr `expect` "Expected an indexing expression"
              Lexer.RSquareBracket `matchOrErr` "Expected closing ]"
              goAccess $ Absyn.Indexing l inner

    parsePrimaryExpr :: ParseRes Absyn.Expr
    parsePrimaryExpr = currToken >>= parsePrimaryExpr'
      where
        parsePrimaryExpr' :: Lexer.Token -> ParseRes Absyn.Expr
        parsePrimaryExpr' (Lexer.IntLit val) =
          eat >> return (Absyn.ConstInt val)
        parsePrimaryExpr' (Lexer.StringLit val) =
          eat >> return (Absyn.ConstStr val)
        parsePrimaryExpr' (Lexer.FloatLit val) =
          eat >> return (Absyn.ConstDouble val)
        parsePrimaryExpr' (Lexer.BoolLit val) =
          eat >> return (Absyn.ConstBool val)
        parsePrimaryExpr' (Lexer.Identifier val) = do
          eat
          sym <- intern val
          return $ Absyn.Identifier sym
        parsePrimaryExpr' Lexer.Nil = eat >> return Absyn.Nil
        parsePrimaryExpr' Lexer.LParen = do
          eat
          expr <- parseExpr `expect` "expected expression inside parenthesis"
          Lexer.RParen `matchOrErr` "unclosed parenthesis"
          return expr
        parsePrimaryExpr' _ = throwError NotThisFn

parseIdentifier :: ParseRes Interner.Symbol
parseIdentifier = do
  curr <- currToken
  case curr of
    Lexer.Identifier name -> do
      eat
      intern name
    _ -> throwError NotThisFn
