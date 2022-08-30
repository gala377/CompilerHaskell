module Unique (
    Unique, Provider, newProvider, createUnique
) where

type Unique = Int

newtype Provider = Provider { count :: Int }

newProvider :: Provider
newProvider = Provider 0

createUnique :: Provider -> (Int, Provider)
createUnique (Provider c) = (c, Provider (c + 1))