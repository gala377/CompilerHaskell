module Syntax.Absyn
  ( Expr (..),
    Program (..),
    Decl (..),
    Type (..),
    TypedName (..),
    testCompareDecls,
    testCompareExpr,
    testComparePrograms,
    testCompareSymbols,
    testCompareType,
    testCompareTypedName,
  )
where

import Data.Text qualified as T
import Syntax.Interner (Symbol, symbolText)

newtype Program = Program [Decl] deriving (Show)

data Decl
  = FunctionDecl
      { name :: Symbol,
        parameters :: [TypedName],
        returnType :: Maybe Type,
        body :: Expr
      }
  | VariableDecl
      { name :: Symbol,
        varType :: Maybe Type,
        initExpr :: Expr
      }
  | TypeDecl Symbol Type
  deriving (Show, Eq)

data Type
  = TypeName Symbol
  | Array Type
  | Record [TypedName]
  deriving (Show, Eq)

data TypedName = TypedName
  { name :: Symbol,
    typ :: Type
  }
  deriving (Show, Eq)

data Expr
  = ConstInt Int
  | ConstDouble Double
  | ConstStr T.Text
  | ConstBool Bool
  | Identifier Symbol
  | {- typeId -> limit -> init -}
    ArrayLit Symbol Expr Expr
  | RecordLit Symbol [(Symbol, Expr)]
  | Nil
  | And Expr Expr
  | Or Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | GtEq Expr Expr
  | LtEq Expr Expr
  | BoolNegate Expr
  | Assignment Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Division Expr Expr
  | Negate Expr
  | Indexing Expr Expr
  | Access Expr Symbol
  | FunctionCall Expr [Expr]
  | Sequence Expr Expr
  | {- If Cond Then Else -}
    If Expr Expr Expr
  | While Expr Expr
  | For {forVar :: Symbol, forVarInit :: Expr, forLimit :: Expr, forBody :: Expr}
  | Let [Decl] Expr
  | Break
  | Continue
  | ErrorExpr
  deriving (Show, Eq)

testComparePrograms :: Program -> Program -> Bool
testComparePrograms (Program decls1) (Program decls2) =
  go decls1 decls2
  where
    go :: [Decl] -> [Decl] -> Bool
    go [] [] = True
    go (d1 : ds1) (d2 : ds2) = testCompareDecls d1 d2 && go ds1 ds2
    go _ _ = False

testCompareDecls :: Decl -> Decl -> Bool
testCompareDecls
  (FunctionDecl {name = name1, parameters = par1, returnType = ret1, body = body1})
  (FunctionDecl {name = name2, parameters = par2, returnType = ret2, body = body2}) =
    testCompareSymbols name1 name2
      && compareRetTyp ret1 ret2
      && compareParams par1 par2
      && testCompareExpr body1 body2
    where
      compareRetTyp :: Maybe Type -> Maybe Type -> Bool
      compareRetTyp (Just t1) (Just t2) = testCompareType t1 t2
      compareRetTyp Nothing Nothing = True
      compareRetTyp _ _ = False

      compareParams p1 p2 =
        (length p1 == length p2)
          && all (uncurry testCompareTypedName) (zip p1 p2)
testCompareDecls
  (VariableDecl {name = name1, varType = varType1, initExpr = initExpr1})
  (VariableDecl {name = name2, varType = varType2, initExpr = initExpr2}) =
    testCompareSymbols name1 name2
      && compareVarType varType1 varType2
      && testCompareExpr initExpr1 initExpr2
    where
      compareVarType :: Maybe Type -> Maybe Type -> Bool
      compareVarType (Just t1) (Just t2) = testCompareType t1 t2
      compareVarType Nothing Nothing = True
      compareVarType _ _ = False
testCompareDecls (TypeDecl n1 t1) (TypeDecl n2 t2) =
  testCompareSymbols n1 n2 && testCompareType t1 t2
testCompareDecls _ _ = False

testCompareTypedName :: TypedName -> TypedName -> Bool
testCompareTypedName
  (TypedName {name = name1, typ = typ1})
  (TypedName {name = name2, typ = typ2}) =
    testCompareSymbols name1 name2
      && testCompareType typ1 typ2

testCompareSymbols :: Symbol -> Symbol -> Bool
testCompareSymbols s1 s2 = symbolText s1 == symbolText s2

testCompareType :: Type -> Type -> Bool
testCompareType (TypeName t1) (TypeName t2) = testCompareSymbols t1 t2
testCompareType (Array t1) (Array t2) = testCompareType t1 t2
testCompareType (Record fs1) (Record fs2) =
  (length fs1 == length fs2)
    && all compareFields (zip fs1 fs2)
  where
    compareFields :: (TypedName, TypedName) -> Bool
    compareFields = uncurry testCompareTypedName
testCompareType _ _ = False

testCompareExpr :: Expr -> Expr -> Bool
testCompareExpr (ConstInt v1) (ConstInt v2) = v1 == v2
testCompareExpr (ConstDouble v1) (ConstDouble v2) = v1 == v2
testCompareExpr (ConstStr v1) (ConstStr v2) = v1 == v2
testCompareExpr (ConstBool v1) (ConstBool v2) = v1 == v2
testCompareExpr (Identifier v1) (Identifier v2) = testCompareSymbols v1 v2
testCompareExpr Nil Nil = True
testCompareExpr Break Break = True
testCompareExpr ErrorExpr ErrorExpr = True
testCompareExpr Continue Continue = True
testCompareExpr (Add e11 e12) (Add e21 e22) =
  testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (Sub e11 e12) (Sub e21 e22) =
  testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (Mult e11 e12) (Mult e21 e22) =
  testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (Division e11 e12) (Division e21 e22) =
  testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (Indexing e11 e12) (Indexing e21 e22) =
  testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (Negate e1) (Negate e2) =
  testCompareExpr e1 e2
testCompareExpr (Access e1 s1) (Access e2 s2) =
  testCompareSymbols s1 s2 && testCompareExpr e1 e2
testCompareExpr (Sequence l1 r1) (Sequence l2 r2) =
  testCompareExpr l1 l2 && testCompareExpr r1 r2
testCompareExpr (FunctionCall e1 p1) (FunctionCall e2 p2) =
  testCompareExpr e1 e2 && length p1 == length p2
    && all (uncurry testCompareExpr) (zip p1 p2)
testCompareExpr (ArrayLit s1 e11 e12) (ArrayLit s2 e21 e22) =
  testCompareSymbols s1 s2 && testCompareExpr e11 e21 && testCompareExpr e12 e22
testCompareExpr (RecordLit s1 l1) (RecordLit s2 l2) =
  testCompareSymbols s1 s2
    && length l1 == length l2
    && all testField (zip l1 l2)
  where
    testField ((n1, t1), (n2, t2)) =
      testCompareSymbols n1 n2 && testCompareExpr t1 t2
testCompareExpr _ _ = False