module Semant.Types (typecheck, Typ (..)) where

import Control.Exception (assert)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS( RWS, runRWS, )
import Control.Monad.Reader (MonadReader(local), asks)
import Control.Monad.State
  ( MonadState,
    State,
    evalState,
    execState,
    get,
    gets,
    modify,
    put,
    runState,
  )
import Control.Monad.Writer (MonadWriter (tell))
import Data.List qualified as List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Debug.Trace (trace)
import Syntax.Absyn qualified as Absyn
import Syntax.Interner (Interner, Symbol, symbolString, symbolText)
import Unique qualified

type SymTable a = Map Symbol a

type TypeId = Unique.Unique

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

data TcState = TcState {uniqP :: Unique.Provider, interner :: Interner}

type TcStateM a = MonadState TcState a

type TcWriterM a = MonadWriter [TypeError] a

type TcReaderM a = MonadReader Env a

type TcM a = RWS Env [TypeError] TcState a

type TypeError = String

{- Env type -}

type Env = (TypeEnv, VarEnv)

typeEnv :: Env -> TypeEnv
typeEnv = fst

varEnv :: Env -> VarEnv
varEnv = snd

{- End Env type -}

{- HasDeclarations type class -}

class HasDeclarations a where
  declarations :: a -> [Absyn.Decl]

instance HasDeclarations Absyn.Program where
  declarations (Absyn.Program decl) = decl

instance HasDeclarations Absyn.Expr where
  declarations (Absyn.Let decl _) = decl
  declarations _ = []

extractVarDecl :: HasDeclarations a => a -> [(Symbol, Absyn.Variable)]
extractVarDecl x = go [] $ declarations x
  where
    go :: [(Symbol, Absyn.Variable)] -> [Absyn.Decl] -> [(Symbol, Absyn.Variable)]
    go acc ((Absyn.VariableDecl s t) : ts) = go ((s, t) : acc) ts
    go acc (_ : ts) = go acc ts
    go acc [] = acc

extractFnDecl :: HasDeclarations a => a -> [(Symbol, Absyn.Function)]
extractFnDecl x = go [] $ declarations x
  where
    go :: [(Symbol, Absyn.Function)] -> [Absyn.Decl] -> [(Symbol, Absyn.Function)]
    go acc ((Absyn.FunctionDecl s t) : ts) = go ((s, t) : acc) ts
    go acc (_ : ts) = go acc ts
    go acc [] = acc

extractTypeDecl :: HasDeclarations a => a -> [(Symbol, Absyn.Type)]
extractTypeDecl x = go [] $ declarations x
  where
    go :: [(Symbol, Absyn.Type)] -> [Absyn.Decl] -> [(Symbol, Absyn.Type)]
    go acc ((Absyn.TypeDecl s t) : ts) = go ((s, t) : acc) ts
    go acc (_ : ts) = go acc ts
    go acc [] = acc

{- End HasDeclarations type class -}

newState :: Unique.Provider -> Interner -> TcState
newState = TcState

typecheck :: Unique.Provider -> Interner -> Absyn.Program -> (TypeEnv, TcState, [TypeError])
typecheck prov int prog = runRWS (typecheck' prog) (Map.empty, Map.empty) (newState prov int)
  where
    typecheck' :: Absyn.Program -> TcM TypeEnv
    typecheck' prog = do
      tEnv <- prepareGlovalTypeEnv prog
      vEnv <- local (const (tEnv, Map.empty)) $ prepareGlovalVarEnv prog
      local (const (tEnv, vEnv)) $ typecheckBodies prog

    typecheckBodies = undefined

    prepareGlovalTypeEnv :: (TcStateM m, TcWriterM m) => Absyn.Program -> m TypeEnv
    prepareGlovalTypeEnv prog = do
      let tdefs = extractTypeDecl prog
      tEnv <- initTypeEnv tdefs
      let env' = resolveNamesToTypes tEnv
      return env'

    prepareGlovalVarEnv :: Absyn.Program -> TcM VarEnv
    prepareGlovalVarEnv prog = do
      let fns = extractFnDecl prog
      let vars = extractVarDecl prog
      fns' <- resolveDecls resolveFunDecl fns
      vars' <- resolveDecls resolveVarDecl vars
      let env = Map.fromList (vars' ++ fns')
      return env

    resolveDecls f =
      traverse
        ( \(n, d) -> do
            d' <- f d
            return (n, d')
        )

    resolveFunDecl (Absyn.Function args ret _) = do
      args' <- traverse resolveTypedName args
      ret' <- traverse resolveType ret
      -- TODO `fromJust` should not be here. Instead the type
      -- of the function should be deduced from its body
      return $ Function args' $ Maybe.fromJust ret'
    resolveVarDecl (Absyn.Variable t _) = do
      t' <- traverse resolveType t
      -- TODO `fromJust` should not be here. Instead the type
      -- of the variable should be deduced from its init expr
      return $ Var $ Maybe.fromJust t'

    resolveTypedName :: Absyn.TypedName -> TcM Typ
    resolveTypedName (Absyn.TypedName _ t) = resolveType t

    resolveType :: Absyn.Type -> TcM Typ
    resolveType t = case t of
      Absyn.TypeName n -> lookUpTypeName n
      Absyn.Array t -> do
        id <- mkUnique
        inner <- resolveType t
        return $ Array inner id
      Absyn.Record fs -> do
        id <- mkUnique
        fs' <-
          traverse
            ( \(Absyn.TypedName n t) -> do
                t' <- resolveType t
                return (n, t')
            )
            fs
        return $ Record fs' id

    lookUpTypeName :: (TcReaderM m, TcWriterM m) => Symbol -> m Typ
    lookUpTypeName name = do
      env <- asks typeEnv
      case Map.lookup name env of
        Just t -> return t
        Nothing -> do
          typeError $ "Unknown type " ++ symbolString name
          return Unit

typeError :: TcWriterM m => String -> m ()
typeError msg = tell [msg]

mkUnique :: TcStateM m => m Unique.Unique
mkUnique = do
  p <- gets uniqP
  let (id, p') = Unique.createUnique p
  modify (\s -> s {uniqP = p'})
  return id

{-
CREATING TYPE ENV FROM DECLARATIONS
-}

initTypeEnv :: TcStateM m => [(Symbol, Absyn.Type)] -> m TypeEnv
initTypeEnv decls = go decls Map.empty
  where
    go :: MonadState TcState m => [(Symbol, Absyn.Type)] -> TypeEnv -> m TypeEnv
    go ((s, t) : ts) env = case Map.lookup s env of
      Nothing -> do
        t' <- transType t
        let env' = Map.insert s t' env
        go ts env'
      Just _ -> error $ "double definition of type " ++ T.unpack (symbolText s)
    go [] env = return env

    transType :: MonadState TcState m => Absyn.Type -> m Typ
    transType (Absyn.TypeName s) = return $ case T.unpack (symbolText s) of
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

    transField :: MonadState TcState m => Absyn.TypedName -> m (Symbol, Typ)
    transField (Absyn.TypedName n t) = do
      t' <- transType t
      return (n, t')

data ResolveKind = Rec | Direct deriving (Ord, Eq, Show)

type ResolveHist = Map Symbol ResolveKind

resolveNamesToTypes :: TypeEnv -> TypeEnv
resolveNamesToTypes tenv =
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
      {- references to type we resolve -} Symbol ->
      {- type to resolve references to -} Typ ->
      {- type to resolve references in -} Typ ->
      {- returned type with resolved references -} Typ
    resolveReferencesInT name t t'@(TypeRef ref)
      | name == ref = t
      | otherwise = t'
    resolveReferencesInT n t (Record fields id) =
      (`Record` id) $ map (mapSnd $ resolveReferencesInT n t) fields
      where
        mapSnd f (x, y) = (x, f y)
    resolveReferencesInT n t (Array t' id) = (`Array` id) $ resolveReferencesInT n t t'
    resolveReferencesInT _ _ t = t