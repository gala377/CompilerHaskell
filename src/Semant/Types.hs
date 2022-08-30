module Semant.Types (typecheck, Typ(..)) where

import Control.Exception (assert)
import Control.Monad.State (State, evalState, execState, get, gets, modify, put, runState)
import Data.List qualified as List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Debug.Trace (trace)
import Syntax.Absyn qualified as Absyn
import Syntax.Interner (Symbol, symbolText)
import Unique qualified

type SymTable a = Map Symbol a

type TypeId = Unique.Unique

type TypeName = Symbol

{- DONT KNOW HOW TO PROGRESS THIS WITHOUT IORef-}
data Typ
  = Int
  | Bool
  | String
  | Double
  | Nil
  | Unit
  | Array Typ TypeId
  | TypeRef Symbol
  | Record [(Symbol, Typ)] TypeId
  deriving (Show)

data VarEnvEntry
  = Var Typ
  | Function [Typ] Typ

type VarEnv = SymTable VarEnvEntry

type TypeEnv = SymTable Typ

data TcState = TcState {tEnv :: TypeEnv, errors :: [String], uniqP :: Unique.Provider}

type TcM a = State TcState a

typecheck :: Unique.Provider -> Absyn.Program -> (TypeEnv, TcState)
typecheck prov prog = runState (typecheck' prog) (newState prov)

typecheck' :: Absyn.Program -> TcM TypeEnv
typecheck' prog = do
  let tdefs = extractTypeDefs prog
  tEnv <- initTypeEnv tdefs
  let env' = trace ("initial tenv: " ++ show tEnv ) resolveSimpleTypeChains tEnv
  return env'

newState :: Unique.Provider -> TcState
newState = TcState Map.empty []

mkUnique :: TcM Unique.Unique
mkUnique = do
  p <- gets uniqP
  let (id, p') = Unique.createUnique p
  modify (\s -> s {uniqP = p'})
  return id

extractTypeDefs :: Absyn.Program -> [(Symbol, Absyn.Type)]
extractTypeDefs p = let Absyn.Program ds = p in go [] ds
  where
    go :: [(Symbol, Absyn.Type)] -> [Absyn.Decl] -> [(Symbol, Absyn.Type)]
    go acc ((Absyn.TypeDecl s t) : ts) = go ((s, t) : acc) ts
    go acc (_ : ts) = go acc ts
    go acc [] = acc

initTypeEnv :: [(Symbol, Absyn.Type)] -> TcM TypeEnv
initTypeEnv decls = go decls Map.empty
  where
    go :: [(Symbol, Absyn.Type)] -> TypeEnv -> TcM TypeEnv
    go ((s, t) : ts) env = case Map.lookup s env of
      Nothing -> do
        t' <- transType t
        let env' = Map.insert s t' env
        go ts env'
      Just _ -> error $ "double definition of type " ++ T.unpack (symbolText s)
    go [] env = return env

transType :: Absyn.Type -> TcM Typ
transType (Absyn.TypeName s) = return t'
  where
    t' = case T.unpack (symbolText s) of
      "int" -> Int
      "bool" -> Bool
      "string" -> String
      "double" -> Double
      "nil" -> Nil
      _ -> TypeRef s
transType (Absyn.Array t) = do
  id <- mkUnique
  t' <- transType t
  return $ Array t' id
transType (Absyn.Record fs) = do
  id <- mkUnique
  fs' <- mapM transField fs
  return $ Record fs' id
  where
    transField :: Absyn.TypedName -> TcM (Symbol, Typ)
    transField (Absyn.TypedName n t) = do
      t' <- transType t
      return (n, t')

data ResolveKind = Rec | Direct deriving (Ord, Eq, Show)

type ResolveHist = Map Symbol ResolveKind 

resolveSimpleTypeChains :: TypeEnv -> TypeEnv
resolveSimpleTypeChains tenv = 
    let env' = resolveSimple tenv
        env'' = resolveReferences env'
     in env''
  where
    resolveSimple env = List.foldl' goResolveSimple env (Map.keys env)
    goResolveSimple e s = snd $ resolveName Map.empty e s

    resolveName :: ResolveHist -> TypeEnv -> Symbol -> (Typ, TypeEnv)
    resolveName hist env name = resolveDef hist env name $ case Map.lookup name env of
      Nothing -> error $ "undefined type " ++ T.unpack (symbolText name)
      Just n -> trace ("for " ++ show name ++ " found type " ++ show n) n

    addToHist name val hist =
      let assertion = assert $ not $ Map.member name hist
       in assertion `seq` Map.insert name val hist

    resolveDef :: ResolveHist -> TypeEnv -> Symbol -> Typ -> (Typ, TypeEnv)
    -- type a = // type a = b
    resolveDef hist env name trt@(TypeRef ref) =
      let hist' = trace ("hist is " ++ show hist) $ trace ("type " ++ T.unpack (symbolText name)) $ addToHist name Direct hist
       in case hist' `seq` trace ("found type alias to " ++ show ref) Map.lookup ref hist' of
            -- did not see this type before, so we can safely resolve it
            Nothing ->
              let (t, env') = resolveName hist' env ref
               in (t, Map.insert name t env')
            -- we saw ref before but it was though the reference
            -- But this is fine, for example:
            -- type a = { foo: ref }
            -- type ref = a
            -- we just bite the bullet and stop here
            Just Rec -> trace "recursive type so resolved to type alias" (trt, env)
            -- we saw this ref before but as a direct definition
            -- for example
            -- type a = b
            -- type b = a
            Just Direct -> error $ "type " ++ T.unpack (symbolText ref) ++ "is part of a definition cycle"
    resolveDef _ env _ t = (t, env)

    resolveReferences :: TypeEnv -> TypeEnv
    resolveReferences env =
        List.foldl' goResolveRefs env $ Map.keys env
      where
        goResolveRefs :: TypeEnv -> Symbol -> TypeEnv
        goResolveRefs env name =
          let typ = env ! name 
              typ' = resolveReferencesInT name typ' typ
              env' = Map.map resolve env
              resolve = resolveReferencesInT name typ'
          in Map.insert name typ' env'

    resolveReferencesInT :: 
      {- references to type we resolve -} Symbol
      -> {- type to resolve references to -} Typ 
      -> {- type to resolve references in -} Typ 
      -> {- returned type with resolved references -} Typ
    resolveReferencesInT name t t'@(TypeRef ref) 
      | name == ref = t
      | otherwise = t'
    resolveReferencesInT n t (Record fields id) = 
        (`Record` id) $ map (mapSnd $ resolveReferencesInT n t) fields
      where
        mapSnd f (x, y) = (x, f y)
    resolveReferencesInT n t (Array t' id) = (`Array` id) $ resolveReferencesInT n t t'
    resolveReferencesInT _  _ t = t