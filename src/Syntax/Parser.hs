module Syntax.Parser (
  parse,
) where

import Control.Monad.State.Lazy
import Data.List qualified as List
import Data.Maybe (
  isJust,
  isNothing,
 )
import Data.Text qualified as T
import Syntax.Absyn qualified as Absyn
import Syntax.Interner qualified as Interner
import Syntax.Lexer qualified as Lexer
import Syntax.Parser.Session
import Control.Monad.Except (runExceptT, MonadError (throwError))
import Data.Bifunctor qualified as Bifunctor


-- this is just ">>=" at this point 
-- passResultOr ::
--   Either EarlyReturn a ->
--   (a -> ParseRes b) ->
--   ParseRes b
-- passResultOr NotThisFn _ = return NotThisFn
-- passResultOr ParsedWithError _ = return ParsedWithError
-- passResultOr (Ok a) f = f a

parse :: String -> [Lexer.Token] -> Either [SyntaxError] Absyn.Program
parse filename tokens = Bifunctor.first (\_ -> syntaxErrors session') parseRes
 where
  session = mkSession filename tokens
  action = runExceptT $ expect parseProgram "Expected top level definition"
  (parseRes, session') = runState action session

parseProgram :: ParseRes Absyn.Program
parseProgram = parseDeclarations []
 where
  parseDeclarations :: [Absyn.Decl] -> ParseRes Absyn.Program
  parseDeclarations decls = do
    curr <- currToken'
    if curr == Lexer.EOF
      then return $ Absyn.Program $ reverse decls
      else do
        decl <- expect parseDecl "expected declaration on the top level"
        parseDeclarations (decl : decls)

  parseDecl :: ParseRes Absyn.Decl
  parseDecl = parseFunDecl `choice` parseVarDecl `choice` parseTypeDecl

  parseTypeDecl = throwError NotThisFn
  parseFunDecl = throwError NotThisFn

  parseVarDecl :: ParseRes Absyn.Decl
  parseVarDecl = do
    currT <- currToken'
    if currT == Lexer.Var
      then do
        eat'
        id <- expect parseIdentifier "expected a name after a var keyword"
        equalSign <- match' Lexer.Assignment
        if isJust equalSign
          then do
            init <- expect parseExpr "expected init expression"
            return $ Absyn.VariableDecl
                    { name = id
                    , varType = Nothing
                    , initExpr = init
                    }
          else do
            addError' $ SyntaxError "Expected an = sign after a variable name"
            throwError ParsedWithError
      else throwError NotThisFn

  parseExpr :: ParseRes Absyn.Expr
  parseExpr = parseAddition

  parseAddition =
    parseBinaryExpr
      [(Lexer.Plus, "+", Absyn.Add), (Lexer.Minus, "-", Absyn.Sub)]
      parseMultiplication

  parseMultiplication =
    parseBinaryExpr
      [ (Lexer.Multiplication, "*", Absyn.Mult)
      , (Lexer.Divide, "/", Absyn.Division)
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
      isToken <- match' t
      if isJust isToken
        then do
          right <- expect next $ "Expected expression after" ++ name
          go (cons left right) options next
        else go left tt next

  parseUnaryExpr :: ParseRes Absyn.Expr
  parseUnaryExpr = do
    t <- currToken'
    case t of
      Lexer.Minus -> do
        eat'
        e <- expect parseUnaryExpr "Expected expression after unary -"
        return $ Absyn.Negate e
      _ -> parsePrimaryExpr

  parsePrimaryExpr :: ParseRes Absyn.Expr
  parsePrimaryExpr = currToken' >>= parsePrimaryExpr'
   where
    parsePrimaryExpr' :: Lexer.Token -> ParseRes Absyn.Expr
    parsePrimaryExpr' (Lexer.IntLit val) =
      eat' >> return (Absyn.ConstInt val)
    parsePrimaryExpr' (Lexer.StringLit val) =
      eat' >> return (Absyn.ConstStr val)
    parsePrimaryExpr' (Lexer.FloatLit val) =
      eat' >> return (Absyn.ConstDouble val)
    parsePrimaryExpr' (Lexer.BoolLit val) =
      eat' >> return (Absyn.ConstBool val)
    parsePrimaryExpr' (Lexer.Identifier val) = do
      eat'
      sym <- intern' val
      return $ Absyn.Identifier sym
    parsePrimaryExpr' Lexer.Nil = eat' >> return Absyn.Nil
    parsePrimaryExpr' Lexer.LParen = do
      eat'
      expr <- expect parseExpr "expected expression inside parenthesis"
      matched <- match' Lexer.RParen
      if isJust matched
        then return expr
        else do
          addError' $ SyntaxError "Unclosed parenthesis"
          throwError ParsedWithError
    parsePrimaryExpr' _ = throwError NotThisFn

  parseIdentifier :: ParseRes Interner.Symbol
  parseIdentifier = do
    curr <- currToken'
    case curr of
      Lexer.Identifier name -> do
        eat'
        intern' name
      _ -> throwError NotThisFn

