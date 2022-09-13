module Lib (
  runExample,
) where

import Interpreter qualified
import Syntax.Lexer qualified
import Syntax.Parser qualified
import Syntax.Parser.Session qualified
import Control.Monad (forever)
import Semant.Types (typecheck)
import qualified Unique
import qualified Syntax.Parser.Session

runExample :: IO ()
runExample = forever $ do
  putStrLn "> input some expression"
  source <- getLine
  let tokens = Syntax.Lexer.toTokens source
  mapM_ print tokens
  let (sess, ast) = Syntax.Parser.runParse "example" tokens
  case ast of
    Left errors -> printErrors errors
    Right ast -> do
      print ("AST: " ++ show ast)
      let uniqP = Unique.newProvider
      let interner = Syntax.Parser.Session.interner sess
      let (_, _, errs) = typecheck uniqP interner ast
      putStrLn "Type errors: "
      mapM_ (\e -> putStrLn $ "type error: " ++ e) errs
    
 where
  printErrors = mapM_ $ putStrLn . Syntax.Parser.Session.unSyntaxError
