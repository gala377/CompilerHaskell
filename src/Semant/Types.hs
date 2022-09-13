module Semant.Types (typecheck, Typ (..)) where

import Control.Exception (assert)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWS, RWST (RWST), runRWS)
import Control.Monad.Reader (MonadReader (local), asks)
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
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable (traverse_)
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
import Syntax.Interner qualified as Interner
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
  | Error -- Error type is always matching
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

typecheck :: Unique.Provider -> Interner -> Absyn.Program -> ((), TcState, [TypeError])
typecheck prov int prog = runRWS (typecheck' prog) (Map.empty, Map.empty) (newState prov int)
  where
    typecheck' :: Absyn.Program -> TcM ()
    typecheck' prog = do
      tEnv <- prepareGlobalTypeEnv prog
      vEnv <- local (const (tEnv, Map.empty)) $ prepareGlobalVarEnv prog
      local (const (tEnv, vEnv)) $ typecheckBodies $ declarations prog

    -- variables are already typechecked
    typecheckBodies :: [Absyn.Decl] -> TcM ()
    typecheckBodies (Absyn.FunctionDecl name (Absyn.Function args _ body) : ds) = do
      venv <- asks varEnv
      let Function pars ret = venv ! name
      let args' = zip (fmap (\(Absyn.TypedName name _) -> name) args) pars
      let venv' = foldl (\env (n, t) -> Map.insert n (Var t) env) venv args'
      local (\(tenv, _) -> (tenv, venv')) $ do
        bodyT <- typecheckExp body
        checkTypesEq ret bodyT "Return type of a function and its body do not match"
      typecheckBodies ds
    typecheckBodies (_ : ds) = typecheckBodies ds
    typecheckBodies [] = return ()

    prepareGlobalTypeEnv :: (TcStateM m, TcWriterM m) => Absyn.Program -> m TypeEnv
    prepareGlobalTypeEnv prog = do
      let tdefs = extractTypeDecl prog
      basicTypes <- basicTypesEnv
      prepareTypeEnv basicTypes tdefs

    basicTypesEnv :: TcStateM m => m (Map Symbol Typ)
    basicTypesEnv = do
      basicTypes <-
        traverse
          (\(name, t) -> intern name >>= \sym -> return (sym, t))
          [ ("int", Int),
            ("bool", Bool),
            ("string", String),
            ("double", Double),
            ("nil", Nil)
          ]
      return $ Map.fromList basicTypes

    prepareGlobalVarEnv :: Absyn.Program -> TcM VarEnv
    prepareGlobalVarEnv prog = do
      let fns = extractFnDecl prog
      let vars = extractVarDecl prog
      fns' <- resolveDecls resolveFunDecl fns
      -- todo: check for duplicates
      let env = Map.fromList fns'
      local (\(tenv, _) -> (tenv, env)) $ do
        vars' <- resolveVarDecl vars
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
      return $ Function args' $ Maybe.fromMaybe Unit ret'

    resolveVarDecl :: [(Symbol, Absyn.Variable)] -> TcM [(Symbol, VarEnvEntry)]
    resolveVarDecl ((name, Absyn.Variable (Just t) body) : tail) = do
      p <- inVarEnv name
      if p then do
        typeError $ "redeclaration of a name " ++ symbolString name
        resolveVarDecl tail
      else do
        t' <- resolveType t
        expT <- typecheckExp body
        checkTypesEq t' expT "body and a declared type of a variable do not match"
        addToVarEnv name t' $ resolveVarDecl tail
    resolveVarDecl ((name, Absyn.Variable Nothing body) : tail) = do
      p <- inVarEnv name
      if p then do
        typeError $ "redeclaration of a name " ++ symbolString name
        resolveVarDecl tail
      else do
        t <- typecheckExp body
        addToVarEnv name t $ resolveVarDecl tail
    resolveVarDecl [] = asks $ Map.toList . varEnv

    addToVarEnv :: Symbol -> Typ -> TcM a -> TcM a
    addToVarEnv name t = local $ Bifunctor.second $ Map.insert name $ Var t

    inVarEnv :: Symbol -> TcM Bool
    inVarEnv name = asks $ Map.member name . varEnv

    resolveTypedName :: Absyn.TypedName -> TcM Typ
    resolveTypedName (Absyn.TypedName _ t) = resolveType t

    resolveType :: Absyn.Type -> TcM Typ
    resolveType (Absyn.TypeName n) = lookUpTypeName n
    resolveType (Absyn.Array t) = do
        id <- mkUnique
        inner <- resolveType t
        return $ Array inner id
    resolveType (Absyn.Record fs) = do
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
          return Error

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

prepareTypeEnv :: TcStateM m => TypeEnv -> [(Symbol, Absyn.Type)] -> m TypeEnv
prepareTypeEnv env decls = do
  env' <- initTypeEnv env decls
  return $ resolveTypeRefs env'

initTypeEnv :: TcStateM m => TypeEnv -> [(Symbol, Absyn.Type)] -> m TypeEnv
initTypeEnv env decls = go decls env
  where
    go :: TcStateM m => [(Symbol, Absyn.Type)] -> TypeEnv -> m TypeEnv
    go ((s, t) : ts) env = case Map.lookup s env of
      Nothing -> do
        t' <- transType env t
        go ts $ Map.insert s t' env
      Just _ -> error $ "double definition of type " ++ T.unpack (symbolText s)
    go [] env = return env

    transType :: MonadState TcState m => TypeEnv -> Absyn.Type -> m Typ
    transType env (Absyn.TypeName s)  = return $ Maybe.fromMaybe (TypeRef s) $ Map.lookup s env
    transType env (Absyn.Array t) = do
      id <- mkUnique
      t' <- transType env t
      return $ Array t' id
    transType env (Absyn.Record fs) = do
      id <- mkUnique
      fs' <- traverse (transField env) fs
      return $ Record fs' id

    transField :: MonadState TcState m => TypeEnv -> Absyn.TypedName -> m (Symbol, Typ)
    transField env (Absyn.TypedName n t) = do
      t' <- transType env t
      return (n, t')

data ResolveKind = Rec | Direct deriving (Ord, Eq, Show)

type ResolveHist = Map Symbol ResolveKind

resolveTypeRefs :: TypeEnv -> TypeEnv
resolveTypeRefs tenv =
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
              env' = Map.map (resolveReferencesInT name typ') env
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

typecheckExp :: Absyn.Expr -> TcM Typ
typecheckExp (Absyn.ConstBool _) = return Bool
typecheckExp (Absyn.ConstDouble _) = return Double
typecheckExp (Absyn.ConstInt _) = return Int
typecheckExp (Absyn.ConstStr _) = return String
typecheckExp Absyn.Nil = return Nil
typecheckExp (Absyn.Identifier name) = do
  env <- asks varEnv
  case Map.lookup name env of
    Nothing -> do
      typeError $ "Unknown name " ++ (symbolString name)
      return Error
    Just (Var t) -> return t
    Just (Function _ _) -> do
      typeError $ "cannot evaluate a function " ++ (symbolString name)
      return Error
typecheckExp (Absyn.ArrayLit t limit init) = do
  arrType <- resolveTypeName t >>= checkArrType
  elemType <- arrElemType arrType
  typecheckExp limit >>= checkLimitType
  initExpT <- typecheckExp init
  if elemType `typesEq` initExpT
    then return arrType
    else do
      typeError "Types mismatched expected array type got something else"
      return arrType
  where
    checkArrType t@(Array _ _) = return t
    checkArrType _ = typeError "Expected an array type in array literal" >> return Error

    arrElemType (Array t _) = return t
    arrElemType _ = return Error

    checkLimitType Int = return ()
    checkLimitType _ = typeError "A size of an array has to be an interger"
typecheckExp (Absyn.RecordLit t fields) = do
  recType <- resolveTypeName t >>= checkRecordType
  case recType of
    Error -> return Error
    Record fieldTypes _ -> do
      let fieldTypeM = Map.fromList fieldTypes
      let litFields = Map.fromList fields
      if Map.keysSet fieldTypeM == Map.keysSet litFields
        then do
          traverse_ (typeCheckField fieldTypeM) fields
          return recType
        else do
          typeError "Not all fields are present in the record literal"
          return recType
  where
    checkRecordType t@(Record _ _) = return t
    checkRecordType _ = typeError "Record literal expects a record type" >> return Error

    typeCheckField :: Map Symbol Typ -> (Symbol, Absyn.Expr) -> TcM ()
    typeCheckField fields (name, init) = do
      let fieldType = fields ! name
      initT <- typecheckExp init
      checkTypesEq fieldType initT "Mismatched type on record literal's field"
typecheckExp Absyn.Break = return Unit
typecheckExp Absyn.Continue = return Unit
typecheckExp (Absyn.Equal left right) = do
  leftType <- typecheckExp left
  rightType <- typecheckExp right
  checkTypesEq leftType rightType "equality expression requires both sides to be of the same type"
  return Bool
typecheckExp (Absyn.NotEqual left right) = typecheckExp $ Absyn.Equal left right
typecheckExp (Absyn.BoolNegate exp) = do
  expType <- typecheckExp exp
  checkTypesEq expType Bool "you can only negate boolean expressions"
  return Bool
typecheckExp (Absyn.And left right) = do
  leftType <- typecheckExp left
  rightType <- typecheckExp right
  checkTypesEq leftType Bool "and expression requires both sides to be bool"
  checkTypesEq rightType Bool "and expression requires both sides to be bool"
  return Bool
typecheckExp (Absyn.Or left right) = do
  leftType <- typecheckExp left
  rightType <- typecheckExp right
  checkTypesEq leftType Bool "or expression requires both sides to be bool"
  checkTypesEq rightType Bool "or expression requires both sides to be bool"
  return Bool
typecheckExp (Absyn.Gt left right) = do
  leftType <- typecheckExp left
  rightType <- typecheckExp right
  checkTypesEq leftType rightType "relational operators have to have bots sides of the same type"
  case leftType of
    Int -> return ()
    Double -> return ()
    Error -> return ()
    _ -> do
      typeError "relational opearator require both sides to be numeric type"
  return Bool

typeCheckExp (Absyn.Lt left right) = typecheckExp (Absyn.Gt left right)
typeCheckExp (Absyn.GtEq left right) = typecheckExp (Absyn.Gt left right)
typeCheckExp (Absyn.LtEq left right) = typecheckExp (Absyn.Gt left right)

intern :: TcStateM m => String -> m Symbol
intern name = do
  i <- gets interner
  let (i', sym) = Interner.intern i $ T.pack name
  modify $ \s -> s {interner = i'}
  return sym

resolveTypeName :: (TcReaderM m, TcWriterM m) => Symbol -> m Typ
resolveTypeName name = do
  env <- asks typeEnv
  case Map.lookup name env of
    Just t -> return t
    Nothing -> do
      typeError $ "Unknow type " ++ symbolString name
      return Error

typesEq :: Typ -> Typ -> Bool
typesEq Int Int = True
typesEq Double Double = True
typesEq String String = True
typesEq Bool Bool = True
typesEq (Record _ _) Nil = True
typesEq Nil (Record _ _) = True
typesEq Nil Nil = True
typesEq (Record _ r1) (Record _ r2) = r1 == r2
typesEq (Array _ a1) (Array _ a2) = a1 == a2
typesEq Error _ = True
typesEq _ Error = True
typesEq _ _ = False

checkTypesEq :: Typ -> Typ -> String -> TcM ()
checkTypesEq exp got msg = do
  if exp `typesEq` got
    then return ()
    else do
      typeError msg
      return ()