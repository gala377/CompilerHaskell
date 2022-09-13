module Syntax.Parser
  ( parse,
    parseExpr,
    runParse,
  )
where

import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.State.Lazy
import Data.Bifunctor qualified as Bifunctor
import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe
  ( isJust,
    isNothing,
  )
import Data.Text qualified as T
import Debug.Trace (trace)
import Syntax.Absyn qualified as Absyn
import Syntax.Interner qualified as Interner
import Syntax.Lexer qualified as Lexer
import Syntax.Parser.Session

runParse :: String -> [Lexer.Token] -> (Session, Either [SyntaxError] Absyn.Program)
runParse filename tokens = (session', parseRes')
  where
    session = mkSession filename tokens
    action = runExceptT $ expect parseProgram "Expected top level definition"
    (parseRes, session') = runState action session
    parseRes' = Bifunctor.first (\_ -> syntaxErrors session') parseRes

parse :: String -> [Lexer.Token] -> Either [SyntaxError] Absyn.Program
parse s t = snd $ runParse s t

parseProgram :: ParseRes Absyn.Program
parseProgram = Absyn.Program <$> parseDeclarations []

parseDeclarations :: [Absyn.Decl] -> ParseRes [Absyn.Decl]
parseDeclarations decls = do
  curr <- currToken
  if (curr == Lexer.EOF) || (curr == Lexer.In)
    then return $ reverse decls
    else
      ( do
          decl <- expect parseDecl "expected declaration"
          parseDeclarations (decl : decls)
      )
  where
    parseDecl :: ParseRes Absyn.Decl
    parseDecl = parseFunDecl `choice` parseVarDecl `choice` parseTypeDecl

    parseTypeDecl = do
      check Lexer.Type
      name <- parseIdentifier `expect` "expected type name"
      Lexer.Assignment `matchOrErr` "expected an equal sign in the type declaration"
      t <- parseType `expect` "expected a type for type declaration"
      return $ Absyn.TypeDecl name t

    parseFunDecl :: ParseRes Absyn.Decl
    parseFunDecl = do
      check Lexer.Fun
      name <- parseIdentifier `expect` "expected function name"
      Lexer.LParen `matchOrErr` "expected functions arg list to be inside parenthesis"
      args <- parseTypedNameList [] `expect` "expected argument list"
      Lexer.RParen `matchOrErr` "expected functions arg list to be inside parenthesis"
      colon <- match Lexer.Colon
      typ <-
        if isJust colon
          then Just <$> (parseType `expect` "expected function return type")
          else return Nothing
      Lexer.Assignment `matchOrErr` "expected = sign after functions declaration"
      body <- parseExpr `expect` "expected functions body"
      return $ Absyn.FunctionDecl name $ Absyn.Function args typ body

    parseTypedName :: ParseRes Absyn.TypedName
    parseTypedName = do
      name <- parseIdentifier
      Lexer.Colon `matchOrErr` "expected a colon"
      typ <- parseType `expect` "expected a type after a colon"
      return $ Absyn.TypedName name typ

    parseTypedNameList :: [Absyn.TypedName] -> ParseRes [Absyn.TypedName]
    parseTypedNameList acc = do
      name <-
        (Just <$> parseTypedName) `catchError` \case
          NotThisFn -> return Nothing
          e -> throwError e
      case name of
        Nothing -> return $ reverse acc
        Just name' -> do
          comma <- match Lexer.Comma
          if isJust comma
            then parseTypedNameList (name' : acc)
            else return $ reverse (name' : acc)

    parseVarDecl :: ParseRes Absyn.Decl
    parseVarDecl = do
      check Lexer.Var
      id <- parseIdentifier `expect` "expected a name after a var keyword"
      colon <- match Lexer.Colon
      typ <-
        if isJust colon
          then Just <$> (parseType `expect` "expected variable type")
          else return Nothing
      Lexer.Assignment `matchExpect` "Expected an = sign after a variable name"
      init <- parseExpr `expect` "expected init expression"
      return $
        Absyn.VariableDecl id $
          Absyn.Variable
            { varType = typ,
              initExpr = init
            }

    parseType :: ParseRes Absyn.Type
    parseType = parseArrType `choice` parseTypeName `choice` parseRecordType
      where
        parseArrType = do
          check Lexer.Array
          Lexer.Of `matchOrErr` "expected 'of' keyword in the array type"
          t <- parseType `expect` "expected a type of an array"
          return $ Absyn.Array t

        parseTypeName = Absyn.TypeName <$> parseIdentifier

        parseRecordType = do
          check Lexer.LBracket
          fields <- parseTypedNameList [] `expect` "expected fields in the record"
          Lexer.RBracket `matchOrErr` "expected closing } in record type"
          return $ Absyn.Record fields

parseExpr :: ParseRes Absyn.Expr
parseExpr = parseSequence
  where
    parseSequence :: ParseRes Absyn.Expr
    parseSequence = do
      left <- parseAssignment
      go left
      where
        go left = do
          s <- match Lexer.Semicolon
          if isJust s
            then do
              right <- parseAssignment `expect` "Expected expression after ;"
              go $ Absyn.Sequence left right
            else return left

    parseAssignment :: ParseRes Absyn.Expr
    parseAssignment = do
      left <- parseStmt
      assign <- match Lexer.Assignment
      if isJust assign
        then do
          right <- parseStmt `expect` "Expected expression after assignment"
          return $ Absyn.Assignment left right
        else return left

    parseStmt :: ParseRes Absyn.Expr
    parseStmt = go parseFns
      where
        parseFns =
          [ (Lexer.If, parseIf),
            (Lexer.While, parseWhile),
            (Lexer.Break, return Absyn.Break),
            (Lexer.Continue, return Absyn.Continue),
            (Lexer.For, parseFor),
            (Lexer.Let, parseLet)
          ]
        go [] = do
          res <- parseEq
          currToken >>= \case
            Lexer.Of -> parseArrLit res
            Lexer.LBracket -> parseRecordLit res
            _ -> return res
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
        _ -> parsePostfixOperators

    parsePostfixOperators =
      postfixParserFrom
        [ (Lexer.Dot, parseAccess),
          (Lexer.LSquareBracket, parseIndexing),
          (Lexer.LParen, parseFnCall)
        ]

    postfixParserFrom :: [(Lexer.Token, Absyn.Expr -> ParseRes Absyn.Expr)] -> ParseRes Absyn.Expr
    postfixParserFrom parsers = do
      left <- parsePrimaryExpr
      go parsers left
      where
        go :: [(Lexer.Token, Absyn.Expr -> ParseRes Absyn.Expr)] -> Absyn.Expr -> ParseRes Absyn.Expr
        go ((tok, p) : ps) left =
          match tok >>= \case
            Nothing -> go ps left
            Just _ -> do
              left' <- p left `expect` "unexpected error"
              go parsers left'
        go [] left = return left

    parseAccess left = do
      field <- parseIdentifier `expect` "Expected an dentifier after access expressio"
      return $ Absyn.Access left field
    parseIndexing left = do
      inner <- parseExpr `expect` "Expected an indexing expression"
      Lexer.RSquareBracket `matchOrErr` "Expected closing ]"
      return $ Absyn.Indexing left inner
    parseFnCall left = do
      arguments <- parseCallArguments [] `expect` "expected function call arguments"
      return $ Absyn.FunctionCall left arguments
      where
        parseCallArguments :: [Absyn.Expr] -> ParseRes [Absyn.Expr]
        parseCallArguments acc = do
          expr <-
            (Just <$> parseExpr) `catchError` \case
              NotThisFn -> return Nothing
              e -> throwError e
          case expr of
            Nothing -> return $ reverse acc
            Just expr' ->
              match Lexer.Comma >>= \case
                Nothing -> return $ reverse (expr' : acc)
                Just _ -> parseCallArguments (expr' : acc)

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

    parseRecordLit :: Absyn.Expr -> ParseRes Absyn.Expr
    parseRecordLit (Absyn.Identifier typeid) = do
      eat
      initList <- parseInitList [] `expect` "expected init list for record literal"
      Lexer.RBracket `matchOrErr` "expected closing bracket on record literal"
      return $ Absyn.RecordLit typeid initList
      where
        parseInitList :: [(Interner.Symbol, Absyn.Expr)] -> ParseRes [(Interner.Symbol, Absyn.Expr)]
        parseInitList acc = do
          name <-
            (Just <$> parseFieldInit) `catchError` \case
              NotThisFn -> return Nothing
              e -> throwError e
          case name of
            Nothing -> return $ reverse acc
            Just name' -> do
              comma <- match Lexer.Comma
              if isJust comma
                then parseInitList (name' : acc)
                else return $ reverse (name' : acc)
        parseFieldInit :: ParseRes (Interner.Symbol, Absyn.Expr)
        parseFieldInit = do
          name <- parseIdentifier
          Lexer.Assignment `matchOrErr` "expected = in a record literal"
          init <- parseExpr `expect` "expected record field init expr"
          return (name, init)
    parseRecordLit _ = do
      addError $ SyntaxError "wrong record literal"
      throwError ParsedWithError

    parseArrLit :: Absyn.Expr -> ParseRes Absyn.Expr
    parseArrLit (Absyn.Indexing (Absyn.Identifier name) limit) = do
      eat
      init <- parseAssignment `expect` "expected init expression in array literal"
      return $ Absyn.ArrayLit name limit init
    parseArrLit _ = do
      addError $ SyntaxError "wrong array literal"
      throwError ParsedWithError

parseIdentifier :: ParseRes Interner.Symbol
parseIdentifier = do
  curr <- currToken
  case curr of
    Lexer.Identifier name -> do
      eat
      intern name
    _ -> throwError NotThisFn
