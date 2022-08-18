module Syntax.ParserSpec (spec) where

import Syntax.Absyn (testCompareDecls)
import Syntax.Absyn qualified as Absyn
import Syntax.Interner (testSymbol)
import Syntax.Lexer qualified as Lexer
import Syntax.Parser qualified as Parser
import Test.Hspec

parse :: String -> Absyn.Program
parse source =
  let tokens = Lexer.toTokens source
   in case Parser.parse "test" tokens of
        Left e -> error ("syntax errors " ++ show e)
        Right r -> r

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses var decl with simple init expr" $ do
      let expected =
            Absyn.VariableDecl
              { name = testSymbol "a"
              , varType = Nothing
              , initExpr = Absyn.ConstInt 1
              }
      let Absyn.Program [res] = parse "var a = 1"
      testCompareDecls res expected `shouldBe` True
  describe "testCompareExpr" $ do
    it "compares addition correctly" $ do
      let e1 = Absyn.Add (Absyn.ConstInt 1) (Absyn.ConstInt 2)
      let e2 = Absyn.Add (Absyn.ConstInt 1) (Absyn.ConstInt 2)
      Absyn.testCompareExpr e1 e2 `shouldBe` True
