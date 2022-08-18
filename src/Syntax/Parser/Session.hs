module Syntax.Parser.Session (
  SyntaxError (..),
  Session (..),
  mkSession,
  ParserState,
  getErrors,
  addError,
  addError',
  getFileName,
  currToken,
  currToken',
  eat,
  eat',
  match,
  match',
  intern,
  intern',
  EarlyReturn (..),
  ParseRes,
  expect,
  choice,
) where

import Control.Monad.Except (
  ExceptT (ExceptT),
  liftEither,
  runExceptT,
  throwError,
 )
import Control.Monad.State.Lazy (State, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Data.List qualified as List
import Data.Text qualified as T
import Syntax.Interner qualified as Interner
import Syntax.Lexer qualified as Lexer

newtype SyntaxError = SyntaxError {unSyntaxError :: String} deriving (Show)

data Session = Session
  { syntaxErrors :: [SyntaxError]
  , filename :: String
  , tokens :: [Lexer.Token]
  , interner :: Interner.Interner
  }

mkSession :: String -> [Lexer.Token] -> Session
mkSession filename tokens =
  Session
    { syntaxErrors = []
    , filename = filename
    , tokens = tokens
    , interner = Interner.newInterner
    }

type ParserState = State Session

getErrors :: ParserState [SyntaxError]
getErrors = gets syntaxErrors

getFileName :: ParserState String
getFileName = gets filename

currToken :: ParserState Lexer.Token
currToken = do
  tt <- gets tokens
  return $ case List.uncons tt of
    Just (t, _) -> t
    Nothing -> Lexer.EOF

currToken' :: ParseRes Lexer.Token
currToken' = lift currToken

match :: Lexer.Token -> ParserState (Maybe Lexer.Token)
match t = do
  curr <- currToken
  if t == curr
    then do
      eat
      return $ Just curr
    else return Nothing

match' :: Lexer.Token -> ParseRes (Maybe Lexer.Token)
match' = lift . match

eat :: ParserState ()
eat = do
  tt <- gets tokens
  case List.uncons tt of
    Just (_, tt') -> modify (\s -> s{tokens = tt'})
    Nothing -> return ()

eat' :: ParseRes ()
eat' = lift eat

intern :: T.Text -> ParserState Interner.Symbol
intern str = do
  s <- get
  let i = interner s
  let (i', sym) = Interner.intern i str
  put (s{interner = i'})
  return sym

intern' :: T.Text -> ParseRes Interner.Symbol
intern' = lift . intern

data EarlyReturn
  = NotThisFn
  | ParsedWithError
  deriving (Eq, Show)

type ParseRes = ExceptT EarlyReturn (State Session)

expect :: ParseRes a -> String -> ParseRes a
expect action err = do
  res <- lift $ runExceptT action
  toErr res
 where
  toErr :: Either EarlyReturn a -> ParseRes a
  toErr (Left NotThisFn) = lift (addError $ SyntaxError err) >> throwError ParsedWithError
  toErr e = liftEither e

choice :: ParseRes a -> ParseRes a -> ParseRes a
choice action alternative = do
  res <- lift $ runExceptT action
  case res of
    Left NotThisFn -> alternative
    other -> liftEither other

addError :: SyntaxError -> ParserState ()
addError msg = modify $ \s -> s{syntaxErrors = msg : syntaxErrors s}

addError' :: SyntaxError -> ParseRes ()
addError' = lift . addError