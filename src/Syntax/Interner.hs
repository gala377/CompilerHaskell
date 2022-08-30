module Syntax.Interner (
  Interner,
  newInterner,
  intern,
  Symbol,
  symbolText,
  getUniqProvider,
  testSymbol,
) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Unique qualified


newtype Symbol = Symbol {unSymbol :: (Unique.Unique, T.Text)}


symbolText :: Symbol -> T.Text
symbolText (Symbol (_, t)) = t

testSymbol :: String -> Symbol
testSymbol s = Symbol (0, T.pack s)

instance Eq Symbol where
  s1 == s2 =
    let (id1, _) = unSymbol s1
        (id2, _) = unSymbol s2
     in id1 == id2

instance Ord Symbol where
  compare (Symbol (i1,_)) (Symbol (i2,_)) = compare i1 i2

instance Show Symbol where
  show (Symbol (_, t)) = "s(" ++ T.unpack t ++ ")"

data Interner = Interner
  { internedStrings :: Map.Map T.Text Symbol
  , uniqueProvider :: Unique.Provider
  }

newInterner :: Interner
newInterner =
  Interner
    { internedStrings = Map.empty
    , uniqueProvider = Unique.newProvider
    }

intern :: Interner -> T.Text -> (Interner, Symbol)
intern int str = case Map.lookup str $ internedStrings int of
  Just s -> (int, s)
  Nothing -> createSymbol
 where
  createSymbol = 
    let Interner{internedStrings, uniqueProvider} = int
        (uniq, provider) = Unique.createUnique uniqueProvider
        s = Symbol (uniq, str)
        newMap = Map.insert str s internedStrings
        newInt = Interner{internedStrings = newMap, uniqueProvider = provider}
     in (newInt, s)

getUniqProvider :: Interner -> Unique.Provider
getUniqProvider (Interner _ u) = u