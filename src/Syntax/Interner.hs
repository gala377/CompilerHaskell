module Syntax.Interner
  ( Interner,
    newInterner,
    intern,
    Symbol,
    symbolText,
    testSymbol
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T

type SymbolId = Int

newtype Symbol = Symbol {unSymbol :: (SymbolId, T.Text)}

symbolText :: Symbol -> T.Text
symbolText (Symbol (_, t)) = t

testSymbol :: String -> Symbol
testSymbol s = Symbol (0, T.pack s)

instance Eq Symbol where
  s1 == s2 =
    let (id1, _) = unSymbol s1
        (id2, _) = unSymbol s2
     in id1 == id2

instance Show Symbol where
  show (Symbol (_, t)) = "s(" ++ T.unpack t ++ ")"

data Interner = Interner
  { internedStrings :: Map.Map T.Text Symbol,
    lastId :: Int
  }

newInterner :: Interner
newInterner =
  Interner
    { internedStrings = Map.empty,
      lastId = 0
    }

intern :: Interner -> T.Text -> (Interner, Symbol)
intern int str = case Map.lookup str $ internedStrings int of
  Just s -> (int, s)
  Nothing -> createSymbol int str
  where
    createSymbol int str =
      let Interner {internedStrings, lastId} = int
          s = Symbol (lastId, str)
          newMap = Map.insert str s internedStrings
          newInt = Interner {internedStrings = newMap, lastId = lastId + 1}
       in (newInt, s)