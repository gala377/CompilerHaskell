module Syntax.Parser.Session
  ( SyntaxError (..),
    Session (..),
    mkSession,
    ParserState,
    getErrors,
    addError,
    getFileName,
    currToken,
    eat,
    match,
    matchExpect,
    matchOrErr,
    ifMatch,
    intern,
    EarlyReturn (..),
    ParseRes,
    check,
    expect,
    choice,
  )
where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    MonadError,
    catchError,
    liftEither,
    runExceptT,
    throwError,
  )
import Control.Monad.State (MonadState, gets, get, put, modify)
import Control.Monad.State.Lazy (State)
import Control.Monad.Trans (lift)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Syntax.Interner qualified as Interner
import Syntax.Lexer qualified as Lexer
import Data.Functor (($>))

newtype SyntaxError = SyntaxError {unSyntaxError :: String} deriving (Show)

data Session = Session
  { syntaxErrors :: [SyntaxError],
    filename :: String,
    tokens :: [Lexer.Token],
    interner :: Interner.Interner
  }

mkSession :: String -> [Lexer.Token] -> Session
mkSession filename tokens =
  Session
    { syntaxErrors = [],
      filename = filename,
      tokens = tokens,
      interner = Interner.newInterner
    }

type ParserState = State Session

type PState m = MonadState Session m

getErrors :: ParserState [SyntaxError]
getErrors = gets syntaxErrors

getFileName :: ParserState String
getFileName = gets filename

-- Get current token without moving forward.
-- Call `eat` to move to the next token.
currToken :: PState m => m Lexer.Token
currToken = do
  tt <- gets tokens
  return $ case List.uncons tt of
    Just (t, _) -> t
    Nothing -> Lexer.EOF

-- If the current token is the same as the passed one
-- the eat it and move forward otherwise return `Nothing`
-- without moving forward.
match :: PState m => Lexer.Token -> m (Maybe Lexer.Token)
match t = do
  curr <- currToken
  if t == curr
    then do
      eat
      return $ Just curr
    else return Nothing

-- Move forward in the token stream.
-- If the stream is empty does nothing
eat :: PState m => m ()
eat = do
  tt <- gets tokens
  case List.uncons tt of
    Just (_, tt') -> modify (\s -> s {tokens = tt'})
    Nothing -> return ()

-- Intern given string and return corresponding symbol.
-- Creates new symbol only if it hasn't been interned before.
intern :: PState m => T.Text -> m Interner.Symbol
intern str = do
  s <- get
  let i = interner s
  let (i', sym) = Interner.intern i str
  put (s {interner = i'})
  return sym

data EarlyReturn
  = NotThisFn
  | ParsedWithError
  deriving (Eq, Show)

type ParseRes = ExceptT EarlyReturn (State Session)

type PRes m = MonadError EarlyReturn m

-- Expect the action to succeed.
-- If not add error with the given message and exit early.
expect :: (PRes m, PState m) => m a -> String -> m a
expect action err =
  action `catchError` \case
    NotThisFn -> do
      addError (SyntaxError err)
      throwError ParsedWithError
    e -> throwError e

-- Expect the token to match.
-- If not add error with the given message and exit early.
matchExpect :: (PRes m, PState m) => Lexer.Token -> String -> m Lexer.Token
matchExpect t msg = check t `expect` msg

-- Expect the token to mach.
-- If not add error with the given message.
-- In contrats to the `matchExpect` this function does not return early.
matchOrErr :: (PRes m, PState m) => Lexer.Token -> String -> m ()
matchOrErr t msg = (check t $> ()) `catchError` \case
  NotThisFn -> do
    addError $ SyntaxError msg
    return ()
  e -> throwError e

ifMatch :: (PRes m, PState m) => Lexer.Token -> m a -> m a -> m a
ifMatch tok iffalse iftrue = do
  t <- match tok
  case t of
    Nothing -> iffalse
    Just _ -> iftrue

-- Check if the current token is equal to the given one.
-- If it is then eat it and succeed.
-- If it isn't then return early with `NotThisFn`.
check :: (PRes m, PState m) => Lexer.Token -> m Lexer.Token
check t = do
  res <- match t
  case res of
    Just t' -> return t'
    Nothing -> throwError NotThisFn

-- Run the first action and if it could
-- not match its production run the other one.
-- If the first action ended the error then the error
-- is propagated and the second action is not run.
choice :: PRes m => m a -> m a -> m a
choice action alternative = do
  action `catchError` \case
    NotThisFn -> alternative
    e -> liftEither $ Left e

-- Add new error.
addError :: PState m => SyntaxError -> m ()
addError msg = modify $ \s -> s {syntaxErrors = msg : syntaxErrors s}
