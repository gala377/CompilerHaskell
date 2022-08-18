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
  matchExpect,
  intern,
  intern',
  EarlyReturn (..),
  ParseRes,
  check,
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
import qualified Data.Maybe as Maybe

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

-- Get current token without moving forward.
-- Call `eat` to move to the next token.
currToken :: ParserState Lexer.Token
currToken = do
  tt <- gets tokens
  return $ case List.uncons tt of
    Just (t, _) -> t
    Nothing -> Lexer.EOF

-- Same as `currToken` but in `ParseRes` monad.
currToken' :: ParseRes Lexer.Token
currToken' = lift currToken

-- If the current token is the same as the passed one
-- the eat it and move forward otherwise return `Nothing`
-- without moving forward.
match :: Lexer.Token -> ParserState (Maybe Lexer.Token)
match t = do
  curr <- currToken
  if t == curr
    then do
      eat
      return $ Just curr
    else return Nothing

-- Same as `match` but in `ParseRes` monad.
match' :: Lexer.Token -> ParseRes (Maybe Lexer.Token)
match' = lift . match

-- Move forward in the token stream.
-- If the stream is empty does nothing
eat :: ParserState ()
eat = do
  tt <- gets tokens
  case List.uncons tt of
    Just (_, tt') -> modify (\s -> s{tokens = tt'})
    Nothing -> return ()

-- Same as `eat` but in `ParseRes` monad.
eat' :: ParseRes ()
eat' = lift eat

-- Intern given string and return corresponding symbol.
-- Creates new symbol only if it hasn't been interned before.
intern :: T.Text -> ParserState Interner.Symbol
intern str = do
  s <- get
  let i = interner s
  let (i', sym) = Interner.intern i str
  put (s{interner = i'})
  return sym

-- Same as `intern` but in `ParseRes` monad.
intern' :: T.Text -> ParseRes Interner.Symbol
intern' = lift . intern

data EarlyReturn
  = NotThisFn
  | ParsedWithError
  deriving (Eq, Show)

type ParseRes = ExceptT EarlyReturn (State Session)

-- Expect the action to succeed.
-- If not add error with the given message and exit early.
expect :: ParseRes a -> String -> ParseRes a
expect action err = do
  res <- lift $ runExceptT action
  toErr res
 where
  toErr :: Either EarlyReturn a -> ParseRes a
  toErr (Left NotThisFn) = lift (addError $ SyntaxError err) >> throwError ParsedWithError
  toErr e = liftEither e

-- Expect the token to match.
-- If not add error with the given message and exit early.
matchExpect :: Lexer.Token -> String -> ParseRes Lexer.Token
matchExpect t msg = check t `expect` msg

-- Check if the current token is equal to the given one.
-- If it is then eat it and succeed.
-- If it isn't then return early with `NotThisFn`.
check :: Lexer.Token -> ParseRes Lexer.Token
check t = do
  res <- match' t
  case res of
    Just t' -> return t'
    Nothing -> throwError NotThisFn

-- Run the first action and if it could
-- not match its production run the other one.
-- If the first action ended the error then the error
-- is propagated and the second action is not run.
choice :: ParseRes a -> ParseRes a -> ParseRes a
choice action alternative = do
  res <- lift $ runExceptT action
  case res of
    Left NotThisFn -> alternative
    other -> liftEither other

-- Add new error.
addError :: SyntaxError -> ParserState ()
addError msg = modify $ \s -> s{syntaxErrors = msg : syntaxErrors s}

-- Same as `addError` but in the `ParseRes` monad.
addError' :: SyntaxError -> ParseRes ()
addError' = lift . addError