module Language.ModPCF.Semantics where

import Language.ModPCF.Environment
import Language.ModPCF.Syntax


--
-- * Evaluation semantics of the core language
--

-- | The value environment.
type ValueEnv = Env Var Value

-- | Values are booleans, integers, and closures.
data Value
   = B Bool
   | I Int
   | Fun ValueEnv Var Expr
  deriving (Eq,Show)

-- | Evaluate a primitive unary operation.
evalP1 :: Op1 -> Value -> Value
evalP1 Not (B b) = B (not b)
evalP1 Neg (I i) = I (negate i)
evalP1 o   v     = typeError "evalP1" [show o, show v]

-- | Evaluate a primitive binary operation.
evalP2 :: Op2 -> Value -> Value -> Value
evalP2 And (B l) (B r) = B (l && r)
evalP2 Or  (B l) (B r) = B (l || r)
evalP2 Add (I l) (I r) = I (l + r)
evalP2 Mul (I l) (I r) = I (l * r)
evalP2 LTE (I l) (I r) = B (l <= r)
evalP2 o   l     r     = typeError "evalP2" [show o, show l, show r]

-- | Valuation function for expressions. Fixpoints are evaluated by
--   expanding to the untypeable Y-combinator, then evaluating.
evalExpr :: ValueEnv -> Expr -> Value
evalExpr _ (LitB b)    = B b
evalExpr _ (LitI i)    = I i
evalExpr m (P1 o e)    = evalP1 o (evalExpr m e)
evalExpr m (P2 o l r)  = evalP2 o (evalExpr m l) (evalExpr m r)
evalExpr m (If c t e)  | B b <- evalExpr m c
                       = evalExpr m (if b then t else e)
evalExpr m (Abs x _ e) = Fun m x e
evalExpr m (App l r)   | Fun m' x e <- evalExpr m l
                       = evalExpr (envAdd x (evalExpr m r) m') e
evalExpr m (Fix e)     = evalExpr m (App ycomb e)
evalExpr m (Ref x)     | Just v <- envGet x m
                       = v
-- evalExpr m (Ext q x)   | Just v <- envGetExt q x m
--                        = v
evalExpr m e           = typeError "evalExpr" [show m, show e]

-- | The Y fixpoint combinator. This function is untypeable in simply typed
--   lambda calculus / PCF, but is used after type checking to implement the
--   fixpoint operation. Note that the boolean type label is arbitrary.
ycomb :: Expr
ycomb = Abs "f" TBool (App term term)
  where
    term = Abs "x" TBool (App (Ref "f") (App (Ref "x") (Ref "x")))

-- | Throw a dynamic type error.
typeError :: String -> [String] -> a
typeError fun args = error
   $ "Dynamic type error!" 
  ++ "  evaluating: " ++ fun
  ++ "  with args:  " ++ show args
  ++ "Did you statically type check the expression first?"
  ++ "  if yes: there's a bug in the type system!"
  ++ "  if no: well, that's your fault!"
