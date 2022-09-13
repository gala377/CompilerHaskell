module Semant.Types (typecheck, Typ (..)) where

import Control.Exception (assert)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWS, RWST (RWST), runRWS)
import Control.Monad.Reader (MonadReader (local), ask, asks)
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
        vars' <- resolveVarDecls vars
        let env = Map.fromList (vars' ++ fns')
        return env

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

prepareTypeEnv :: (TcStateM m, TcWriterM m) => TypeEnv -> [(Symbol, Absyn.Type)] -> m TypeEnv
prepareTypeEnv env decls = do
  env' <- initTypeEnv env decls
  resolveTypeRefs env'

initTypeEnv :: (TcStateM m, TcWriterM m) => TypeEnv -> [(Symbol, Absyn.Type)] -> m TypeEnv
initTypeEnv env decls = go decls env
  where
    go :: (TcStateM m, TcWriterM m) => [(Symbol, Absyn.Type)] -> TypeEnv -> m TypeEnv
    go ((s, t) : ts) env = case Map.lookup s env of
      Nothing -> do
        t' <- transType env t
        go ts $ Map.insert s t' env
      Just _ -> do
        typeError $ "double definition of type " ++ T.unpack (symbolText s)
        go ts env
    go [] env = return env

    transType :: MonadState TcState m => TypeEnv -> Absyn.Type -> m Typ
    transType env (Absyn.TypeName s) = return $ Maybe.fromMaybe (TypeRef s) $ Map.lookup s env
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

resolveTypeRefs :: TcWriterM m => TypeEnv -> m TypeEnv
resolveTypeRefs tenv = do
  env' <- resolveSimple tenv
  return $ resolveReferences env'
  where
    resolveSimple :: TcWriterM m => TypeEnv -> m TypeEnv
    resolveSimple env = foldM goResolveSimple env (Map.keys env)
    goResolveSimple :: TcWriterM m => TypeEnv -> Symbol -> m TypeEnv
    goResolveSimple e s = do
      (_, e') <- resolveName Map.empty e s
      return e'

    resolveName :: TcWriterM m => ResolveHist -> TypeEnv -> Symbol -> m (Typ, TypeEnv)
    resolveName hist env name = do
      t <- case Map.lookup name env of
        Nothing -> do
          typeError $ "undefined type " ++ T.unpack (symbolText name)
          return Error
        Just n -> return n
      resolveDef hist env name t

    addToHist name val hist =
      let assertion = assert $ not $ Map.member name hist
       in assertion `seq` Map.insert name val hist

    resolveDef :: TcWriterM m => ResolveHist -> TypeEnv -> Symbol -> Typ -> m (Typ, TypeEnv)
    -- type a = // type a = b
    resolveDef hist env name trt@(TypeRef ref) =
      let hist' = trace ("hist is " ++ show hist) $ trace ("type " ++ T.unpack (symbolText name)) $ addToHist name Direct hist
       in case hist' `seq` trace ("found type alias to " ++ show ref) Map.lookup ref hist' of
            -- did not see this type before, so we can safely resolve it
            Nothing -> do
              (t, env') <- resolveName hist' env ref
              return (t, Map.insert name t env')
            -- we saw ref before but it was though the reference
            -- But this is fine, for example:
            -- type a = { foo: ref }
            -- type ref = a
            -- we just bite the bullet and stop here
            Just Rec -> return $ trace "recursive type so resolved to type alias" (trt, env)
            -- we saw this ref before but as a direct definition
            -- for example
            -- type a = b
            -- type b = a
            Just Direct -> do
              typeError $ "type " ++ T.unpack (symbolText ref) ++ "is part of a definition cycle"
              return (Error, env)
    resolveDef _ env _ t = return (t, env)

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

resolveVarDecls :: [(Symbol, Absyn.Variable)] -> TcM [(Symbol, VarEnvEntry)]
resolveVarDecls ((name, Absyn.Variable (Just t) body) : tail) = do
  p <- inVarEnv name
  if p
    then do
      typeError $ "redeclaration of a name " ++ symbolString name
      resolveVarDecls tail
    else do
      t' <- resolveType t
      expT <- typecheckExp body
      checkTypesEq t' expT "body and a declared type of a variable do not match"
      addToVarEnv name t' $ resolveVarDecls tail
resolveVarDecls ((name, Absyn.Variable Nothing body) : tail) = do
  p <- inVarEnv name
  if p
    then do
      typeError $ "redeclaration of a name " ++ symbolString name
      resolveVarDecls tail
    else do
      t <- typecheckExp body
      addToVarEnv name t $ resolveVarDecls tail
resolveVarDecls [] = asks $ Map.toList . varEnv

addToVarEnv :: Symbol -> Typ -> TcM a -> TcM a
addToVarEnv name t = local $ Bifunctor.second $ Map.insert name $ Var t

inVarEnv :: Symbol -> TcM Bool
inVarEnv name = asks $ Map.member name . varEnv

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

resolveTypedName :: Absyn.TypedName -> TcM Typ
resolveTypedName (Absyn.TypedName _ t) = resolveType t

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
typecheckExp (Absyn.Lt left right) = typecheckExp (Absyn.Gt left right)
typecheckExp (Absyn.GtEq left right) = typecheckExp (Absyn.Gt left right)
typecheckExp (Absyn.LtEq left right) = typecheckExp (Absyn.Gt left right)
typecheckExp letExpr@(Absyn.Let _ body) = do
  let types = extractTypeDecl letExpr
  let functions = extractFnDecl letExpr
  let vars = extractVarDecl letExpr
  (tenv, venv) <- ask
  tenv' <- prepareTypeEnv tenv types
  local (\(_, ve) -> (tenv', ve)) $ do
    functions' <- resolveDecls resolveFunDecl functions
    vars' <- resolveDecls resolveNoRecVars vars
    let venv' = List.foldl' (\e (n, f) -> Map.insert n f e) venv functions'
    let venv'' = List.foldl' (\e (n, v) -> Map.insert n v e) venv' vars'
    local (const (tenv', venv'')) $ typecheckExp body
  where
    resolveNoRecVars (Absyn.Variable (Just t) body) = do
      t <- resolveType t
      expT <- typecheckExp body
      checkTypesEq t expT "Variables type and its body type do not match"
      return $ Var t
    resolveNoRecVars (Absyn.Variable Nothing body) = do
      t <- typecheckExp body
      return $ Var t
typecheckExp (Absyn.Assignment left right) = do
  rightT <- typecheckExp right
  leftT <- case left of
    ident@(Absyn.Identifier _) -> typecheckExp ident
    accs@(Absyn.Access _ _) -> typecheckExp accs
    indx@(Absyn.Indexing _ _) -> typecheckExp indx
    _ -> do
      typeError "left side of an assignment is not an lvalue"
      return Error
  checkTypesEq leftT rightT "assignment types mismatched"
  return Unit
typecheckExp (Absyn.Add left right) = do
  leftT <- typecheckExp left
  rightT <- typecheckExp right
  let compTypes = Maybe.isJust $ List.find (typesEq leftT) [Int, Double, String]
  if compTypes
    then do
      checkTypesEq leftT rightT "both sides of add need to have the same type"
      return leftT
    else do
      typeError "This type does not support addition"
      return Error
typecheckExp (Absyn.Sub left right) = do
  leftT <- typecheckExp left
  rightT <- typecheckExp right
  let compTypes = Maybe.isJust $ List.find (typesEq leftT) [Int, Double, String]
  if compTypes
    then do
      checkTypesEq leftT rightT "both sides of sub need to have the same type"
      return leftT
    else do
      typeError "This type does not support substraction"
      return Error
typecheckExp (Absyn.Division left right) = do
  leftT <- typecheckExp left
  rightT <- typecheckExp right
  let compTypes = Maybe.isJust $ List.find (typesEq leftT) [Int, Double]
  if compTypes
    then do
      checkTypesEq leftT rightT "both sides of div need to have the same type"
      return leftT
    else do
      typeError "This type does not support division"
      return Error
typecheckExp (Absyn.Mult left right) = do
  leftT <- typecheckExp left
  rightT <- typecheckExp right
  let compTypes = Maybe.isJust $ List.find (typesEq leftT) [Int, Double]
  if compTypes
    then do
      checkTypesEq leftT rightT "both sides of mult need to have the same type"
      return leftT
    else do
      typeError "This type does not support multiplication"
      return Error
typecheckExp (Absyn.Negate body) = do
  t <- typecheckExp body
  case t of
    Int -> return Int
    Double -> return Double
    Error -> return Error
    _ -> do
      typeError "only numeric types can be negated"
      return Error
typecheckExp (Absyn.Indexing arr idx) = do
  arrT <- typecheckExp arr
  idx <- typecheckExp idx
  case idx of
    Int -> return ()
    Error -> return ()
    _ -> do
      typeError "Array index has to be an integer"
      return ()
  case arrT of
    Error -> return Error
    Array elemT _ -> return elemT
    _ -> do
      typeError "Only arrays can be indexed"
      return Error
typecheckExp (Absyn.Access left field) = do
  leftT <- typecheckExp left
  case leftT of
    Record fields _ -> case extractField fields of
      Just t -> return t
      Nothing -> do
        typeError $ "this record does not have a field " ++ symbolString field
        return Error
    Error -> return Error
    _ -> do
      typeError "access can only be done on records"
      return Error
  where
    extractField :: [(Symbol, Typ)] -> Maybe Typ
    extractField [] = Nothing
    extractField ((s, t) : rest) | s == field = Just t
    extractField (_ : rest) = extractField rest
typecheckExp (Absyn.FunctionCall func pars) = do
  case func of
    Absyn.Identifier name -> do
      funcEntry <- asks $ Map.lookup name . varEnv
      case funcEntry of
        Just (Function parT retT) -> do
          if length parT == length pars
            then do
              traverse_ 
                (\(t, e) -> do t' <- typecheckExp e; checkTypesEq t' t "mismatched type of function call argument")
                (zip parT pars)
              return retT
            else do
              typeError $ "mismatched number of arguments when calling a function " ++ symbolString name
              return Error
        Just (Var _) -> do
          typeError "trying to call a variable and not a function"
          return Error
        Nothing -> do
          typeError $ "Unknown identifier " ++ symbolString name
          return Error
    _ -> do
      typeError "Currently only names can be called"
      return Error
typecheckExp (Absyn.Sequence curr next) = do
  typecheckExp curr
  typecheckExp next
typecheckExp (Absyn.If cond pos neg) = do
  condT <- typecheckExp cond
  checkTypesEq condT Bool "if's condition has to be a boolean"
  posT <- typecheckExp pos
  negT <- typecheckExp neg
  checkTypesEq posT negT "if's then and else branch need to have the same type"
  return posT
typecheckExp (Absyn.While cond body) = do
  condT <- typecheckExp cond
  checkTypesEq condT Bool "while's condition has to be a boolean"
  typecheckExp body
  return Unit
typecheckExp (Absyn.For var init limit body) = do
  initT <- typecheckExp init
  checkTypesEq initT Int "init expression of a for loop has to ben an integer"
  limitT <- typecheckExp limit
  checkTypesEq limitT Int "limit expression of a for loop has to be an interger"
  addToVarEnv var Int $ typecheckExp body
  return Unit
typecheckExp Absyn.ErrorExpr = return Error

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