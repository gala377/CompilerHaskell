module Syntax.InternerSpec (spec) where

import Test.Hspec

import Data.Text qualified as T
import Syntax.Interner qualified as Interner

spec :: Spec
spec = do
    describe "intern" $ do
        it "creates a new symbol for the given string" $ do
            let i = Interner.newInterner
            let (_, s) = Interner.intern i $ T.pack "sym"
            Interner.symbolText s `shouldBe` T.pack "sym"
        it "returns the same symbol for the same string" $ do
            let i = Interner.newInterner
            let (i', s) = Interner.intern i $ T.pack "sym"
            let (_, s') = Interner.intern i' $ T.pack "sym"
            s `shouldBe` s'
        it "returns different symbols for different strings" $ do
            let i = Interner.newInterner
            let (i', s) = Interner.intern i $ T.pack "sym1"
            let (_, s') = Interner.intern i' $ T.pack "sym2"
            s `shouldNotBe` s'


