module Language.ModPCF.TypeCheck where

import Language.ModPCF.Environment
import Language.ModPCF.Syntax
import Language.ModPCF.TypeResult


--
-- * Typing the core language
--

-- | The typing environment.
type TypeEnv = Env Var Type

-- | Type a primitive unary operation.
typeP1 :: Op1 -> Type -> Maybe Type
typeP1 Not TBool = Just TBool
typeP1 Neg TInt  = Just TInt 
typeP1 _   _     = Nothing

-- | Type a primitive binary operation.
typeP2 :: Op2 -> Type -> Type -> Maybe Type
typeP2 And TBool TBool = Just TBool
typeP2 Or  TBool TBool = Just TBool
typeP2 Add TInt  TInt  = Just TInt
typeP2 Mul TInt  TInt  = Just TInt
typeP2 LTE TInt  TInt  = Just TBool
typeP2 _   _     _     = Nothing

-- | Typing relation for expressions.
typeExpr :: TypeEnv -> Expr -> Result Type

typeExpr _ (LitB _) = return TBool
typeExpr _ (LitI _) = return TInt

typeExpr m this@(P1 o e) = do
    t <- typeExpr m e
    case typeP1 o t of
      Just t -> return t
      _ -> mismatch this [(e,t)]

typeExpr m this@(P2 o l r) = do
    lt <- typeExpr m l
    rt <- typeExpr m r
    case typeP2 o lt rt of
      Just t -> return t
      _ -> mismatch this [(l,lt),(r,rt)]

typeExpr m this@(If c t e) = do
    ct <- typeExpr m c
    tt <- typeExpr m t
    et <- typeExpr m e
    case ct of
      TBool | tt == et -> return tt
      _ -> mismatch this [(c,ct),(t,tt),(e,et)]

typeExpr m (Abs x t e) = do
    res <- typeExpr (envAdd x t m) e
    return (t :-> res)

typeExpr m this@(App l r) = do
    lt <- typeExpr m l
    rt <- typeExpr m r
    case lt of
      arg :-> res | rt == arg -> return res
      _ -> mismatch this [(l,lt),(r,rt)]

typeExpr m this@(Fix e) = do
    t <- typeExpr m e
    case t of
      arg :-> res | arg == res -> return res
      _ -> mismatch this [(e,t)]

typeExpr m this@(Ref x) = do
    case envGet x m of
      Just t -> return t
      _ -> notFound this

-- typeExpr m this@(Ext q x) = do
--     case envGetExt q x m of
--       Just t -> return t
--       _ -> notFound this
