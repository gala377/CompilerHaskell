module Syntax.ParserSpec (spec) where

import Control.Monad.State.Lazy (State, runState)
import Control.Monad.Except (runExceptT)

import Text.Pretty.Simple (pShow)

import Syntax.Absyn (testCompareDecls, testCompareExpr)
import Syntax.Absyn qualified as Absyn
import Syntax.Interner (testSymbol)
import Syntax.Lexer qualified as Lexer
import Syntax.Parser qualified as Parser
import Syntax.Parser.Session qualified as Session
import Test.Hspec
import Test.HUnit (assertBool)
import Data.Text.Lazy qualified as T

parse :: String -> Absyn.Program
parse source =
  let tokens = Lexer.toTokens source
   in case Parser.parse "test" tokens of
        Left e -> error ("syntax errors " ++ show e)
        Right r -> r
parseExpr :: String -> Absyn.Expr
parseExpr source = case parseRes of
    Right r -> r
    Left _ -> error ("syntax errors " ++ show (Session.syntaxErrors session'))
  where
    tokens = Lexer.toTokens source
    session = Session.mkSession "test" tokens
    action = runExceptT Parser.parseExpr
    (parseRes, session') = runState action session

pretty :: (Show a) => a -> String
pretty = T.unpack . pShow

astShouldBe :: (Show a) => (a -> a -> Bool) -> a -> a -> Expectation
astShouldBe p got want = assertBool msg (p want got)
  where
    msg = "Expected:\n\n" ++ pretty want ++ "\n\nGot:\n\n" ++ pretty got

exprShouldBe = astShouldBe testCompareExpr
declShouldBe = astShouldBe testCompareDecls

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
      res `declShouldBe` expected
    it "parses a function declaration" $ do
      let expected =
            Absyn.FunctionDecl
              (testSymbol "foo")
              [Absyn.TypedName (testSymbol "arg") $ Absyn.TypeName $ testSymbol "argt"]
              (Just $ Absyn.TypeName  $ testSymbol "ret")
              (Absyn.ConstInt 10)
      let Absyn.Program [res] = parse "function foo(arg: argt): ret = 10"
      res `declShouldBe` expected
    it "parser a type decl" $ do
      let expected = Absyn.TypeDecl (testSymbol "foo") $ Absyn.Array $ Absyn.TypeName $ testSymbol "int"
      let Absyn.Program [res] = parse "type foo = array of int"
      res `declShouldBe` expected
  describe "parseExpr" $ do
    it "parses simple access" $ do
      let expected = Absyn.Access (Absyn.Identifier $ testSymbol "a") (testSymbol "b")
      let res = parseExpr "a.b"
      res `exprShouldBe` expected
    it "parses access chain" $ do
      let expected =
            Absyn.Access
              (Absyn.Access 
                (Absyn.Access 
                    (Absyn.Identifier $ testSymbol "a")
                    (testSymbol "b"))
                (testSymbol "c"))
              (testSymbol "d")
      let res = parseExpr "a.b.c.d"
      res `exprShouldBe` expected
    it "parses indexing" $ do
      let expected =
            Absyn.Indexing
              (Absyn.Identifier (testSymbol "a"))
              (Absyn.ConstInt 100)
      let res = parseExpr "a[100]"
      res `shouldBe` expected
    it "parses access chain with indexing" $ do
      let expected =
            Absyn.Indexing
              (Absyn.Access
                (Absyn.Access 
                  (Absyn.Indexing 
                      (Absyn.Identifier $ testSymbol "a")
                      (Absyn.Add 
                        (Absyn.Identifier $ testSymbol "b")
                        (Absyn.ConstInt 100)))
                  (testSymbol "c"))
                (testSymbol "d"))
              (Absyn.ConstInt 100)
      let res = parseExpr "a[b + 100].c.d[100]"
      res `exprShouldBe` expected
    it "parses access chain with indexing and unary operators" $ do
      let expected =
            Absyn.Negate $
              Absyn.Negate $
                Absyn.Indexing
                  (Absyn.Access
                    (Absyn.Access 
                      (Absyn.Indexing 
                          (Absyn.Identifier $ testSymbol "a")
                          (Absyn.Add 
                            (Absyn.Identifier $ testSymbol "b")
                            (Absyn.ConstInt 100)))
                      (testSymbol "c"))
                    (testSymbol "d"))
                  (Absyn.ConstInt 100)
      let res = parseExpr "--a[b + 100].c.d[100]"
      res `exprShouldBe` expected
    it "parses array literal" $ do
      let expected = Absyn.ArrayLit (testSymbol "a") (Absyn.ConstInt 100) (Absyn.ConstInt 200)
      let res = parseExpr "a[100] of 200"
      res `exprShouldBe` expected
    it "parses complex record literal" $ do
      let expected = 
              Absyn.RecordLit 
                (testSymbol "a")
                [ (testSymbol "b", Absyn.ConstInt 100)
                , (testSymbol "c", Absyn.ArrayLit (testSymbol "d") (Absyn.ConstInt 100) (Absyn.ConstInt 200))
                ]
      let res = parseExpr "a { b = 100, c = d[100] of 200 }"
      res `exprShouldBe` expected
    it "parses a function call" $ do
      let expected = 
              Absyn.FunctionCall
                (Absyn.Access (Absyn.Identifier $ testSymbol "a") (testSymbol "b"))
                [ Absyn.Add (Absyn.ConstInt 1) (Absyn.ConstInt 2)
                , Absyn.ConstInt 3
                ]
      let res = parseExpr "a.b(1+2, 3)"
      res `exprShouldBe` expected

  describe "testCompareExpr" $ do
    it "compares addition correctly" $ do
      let e1 = Absyn.Add (Absyn.ConstInt 1) (Absyn.ConstInt 2)
      let e2 = Absyn.Add (Absyn.ConstInt 1) (Absyn.ConstInt 2)
      Absyn.testCompareExpr e1 e2 `shouldBe` True
