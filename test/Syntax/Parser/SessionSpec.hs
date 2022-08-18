module Syntax.Parser.SessionSpec (spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Syntax.Parser.Session
import Test.Hspec

newSession :: Session
newSession = mkSession "" []

spec :: Spec
spec = do
  describe "addError" $ do
    it "adds an error" $ do
      let Session{syntaxErrors = errs, ..} = execState (addError $ SyntaxError "test") newSession
      unSyntaxError <$> errs `shouldBe` ["test"]
    it "adds multiple errors" $ do
      let Session{syntaxErrors = errs, ..} = execState (addError (SyntaxError "test1") >> addError (SyntaxError "test2")) newSession
      unSyntaxError <$> errs `shouldBe` ["test2", "test1"]
  describe "except" $ do
    it "preserves results" $ do
      let res = evalState (runExceptT (expect (return "test") "should not happen")) newSession
      res `shouldBe` Right "test"
    it "preserves parse errors" $ do
      let action = runExceptT (expect (throwError ParsedWithError) "should not happen")
      let res :: Either EarlyReturn () = evalState action newSession
      res `shouldBe` Left ParsedWithError
    it "maps early return to error" $ do
      let action = runExceptT (expect (throwError NotThisFn) "test")
      let res :: Either EarlyReturn () = evalState action newSession
      res `shouldBe` Left ParsedWithError
    it "adds error to state on early return" $ do
      let action = runExceptT (expect (throwError NotThisFn) "test")
      let Session{syntaxErrors = err, ..} = execState action newSession
      unSyntaxError <$> err `shouldBe` ["test"]
    it "short cuircuits on multiple actions" $ do
      let action = runExceptT $ expect (throwError NotThisFn) "test1" >> expect (throwError NotThisFn) "test2"
      let Session{syntaxErrors = err, ..} = execState action newSession
      unSyntaxError <$> err `shouldBe` ["test1"]