module Interpreter 
    ( Value ( .. )
    , eval
    ) where 

import qualified Syntax.Absyn as Absyn

data Value
    = IntVal Int
    | FloatVal Float
    | StringVal String
    | Null
    deriving Show

eval :: Absyn.Expr -> Value
eval (Absyn.ConstInt v) = IntVal v
eval (Absyn.Add left right) =
  let IntVal left' = eval left
      IntVal right' = eval right
  in IntVal (left' + right')
eval (Absyn.Sub left right) =
  let IntVal left' = eval left
      IntVal right' = eval right
  in IntVal (left' - right')
eval (Absyn.Mult left right) =
  let IntVal left' = eval left
      IntVal right' = eval right
  in IntVal (left' * right')
eval (Absyn.Division left right) =
  let IntVal left' = eval left
      IntVal right' = eval right
  in IntVal (left' `div` right')
eval (Absyn.Negate e) =
  let IntVal e' = eval e in IntVal (-e')
eval _ = unimplemented

unimplemented = error "unimplemented"

