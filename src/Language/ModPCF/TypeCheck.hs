module Language.ModPCF.TypeCheck where

import Language.ModPCF.Environment
import Language.ModPCF.Signature
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
      _ -> noSuchVar this

-- typeExpr m this@(Ext q x) = do
--     case envGetExt q x m of
--       Just t -> return t
--       _ -> notFound this


--
-- * Typing the module system
--

-- | The signature environment associates signatures with module names.
type SigEnv = Env MVar Signature

-- | Load a declaration into a signature.
loadDecl :: SigEnv -> Signature -> Decl -> Result Signature
loadDecl _   sig this@(DType x Abstract)
    | sigHasType x sig = duplicateTVar this
    | otherwise = return (sigAddType x (TRef x) sig)
loadDecl ext sig (DType x (Concrete t)) = do
    t' <- expandType ext sig t
    return (sigAddType x t' sig)
loadDecl ext sig (DVal x t) = do
    t' <- expandType ext sig t
    return (sigAddVal x t' sig)

-- | Expand all type synonyms in a type.
expandType :: SigEnv -> Signature -> Type -> Result Type
expandType _ _ TBool = return TBool
expandType _ _ TInt  = return TInt
expandType ext sig (arg :-> res) = do
    arg' <- expandType ext sig arg
    res' <- expandType ext sig res
    return (arg' :-> res')
expandType _ sig this@(TRef x)
    | Just t <- sigGetType x sig = return t
    | otherwise = noSuchTVar this
expandType ext _ this@(TExt m x)
    | Just t <- envGet m ext >>= sigGetType x = return t
    | otherwise = noSuchTVar this
