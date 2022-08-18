module Lib (
  runExample,
) where

import Interpreter qualified
import Syntax.Lexer qualified
import Syntax.Parser qualified
import Syntax.Parser.Session qualified

runExample :: IO ()
runExample = do
  putStrLn "> input some expression"
  source <- getLine
  let tokens = Syntax.Lexer.toTokens source
  mapM_ print tokens
  let ast = Syntax.Parser.parse "example" tokens
  case ast of
    Left errors -> printErrors errors
    Right val -> do
      print val
 where
  printErrors = mapM_ $ putStrLn . Syntax.Parser.Session.unSyntaxError
