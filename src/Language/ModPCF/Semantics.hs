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
  ++ "Did you statically type check and link the expression first?"
  ++ "  if yes: there's a bug in the type system and/or linker!"
  ++ "  if no: well, that's your fault!"


--
-- * Linking
--

-- | At link time, a module is just its bare value bindings, that is a
--   mapping from variables to the expressions they're bound to.
type Module = Env Var Expr

-- | The external environment maps module names to modules.
type ModEnv = Env MVar Module

-- | Load a binding into a module.
loadBind :: ModEnv -> Module -> Bind -> Module
loadBind _   mod (BType _ _) = mod
loadBind ext mod (BVal v e)  = envAdd v (linkExpr ext mod e) mod

-- | Link an expression by patching in the expressions bound by value bindings
--   in modules. The module environment is used to patch external references.
--   Local references may be patched using bindings in the local module.
--   Note that abstractions can shadow bindings in the local module.
linkExpr :: ModEnv -> Module -> Expr -> Expr
linkExpr ext mod this = case this of
    LitB b    -> LitB b
    LitI i    -> LitI i
    P1 o e    -> P1 o (link e)
    P2 o l r  -> P2 o (link l) (link r)
    If c t e  -> If (link c) (link t) (link e)
    Abs x t e -> Abs x t (linkExpr ext (envDel x mod) e)
    App l r   -> App (link l) (link r)
    Fix e     -> Fix (link e)
    Ref x     -> maybe err id (envGet x mod)
    Ext q x   -> maybe err id (envGet q ext >>= \m -> envGet x m)
  where
    link = linkExpr ext mod
    err  = linkError "linkExpr" [show ext, show mod, show this]

-- | Link a module expression by linking all of its component expressions and
--   generating a corresponding module.
linkMExpr :: ModEnv -> MExpr -> Module
linkMExpr ext (Mod bs) = foldl (loadBind ext) envEmpty bs
linkMExpr ext (MRef m)
    | Just mod <- envGet m ext = mod
    | otherwise = linkError "linkMExpr" [show ext, show (MRef m)]

-- | Link a top-level binding. For modules add a corresponding entry to the
--   module environment. Ignore signatures, which are only relevant during
--   type checking.
linkTop :: ModEnv -> Top -> ModEnv
linkTop ext (TMod m _ e) = envAdd m (linkMExpr ext e) ext
linkTop ext (TSig _ _)   = ext

-- | Link a program into a single core language expression. The resulting
--   expression will not contain external references and can be evaluated
--   with an empty environment.
linkProg :: Prog -> Expr
linkProg (Prog bs e) = linkExpr (foldl linkTop envEmpty bs) envEmpty e

-- | Throw a dynamic linker error.
linkError :: String -> [String] -> a
linkError fun args = error
   $ "Linker error!"
  ++ "  evaluating: " ++ fun
  ++ "  with args:  " ++ show args
  ++ "Did you statically type check the expression first?"
  ++ "  if yes: there's a bug in the type system!"
  ++ "  if no: well, that's your fault!"
