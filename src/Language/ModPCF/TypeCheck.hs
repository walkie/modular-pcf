module Language.ModPCF.TypeCheck where

import Control.Monad (foldM)

import Language.ModPCF.Environment
import Language.ModPCF.Signature
import Language.ModPCF.Syntax
import Language.ModPCF.TypeResult


--
-- * Typing the core language
--

-- | Expand a type to remove type synonyms. After expansion only remaining
--   type variables correspond to abstract types. The range of a signature
--   signature contain only expanded types. During expansion, a signature
--   environment is used to resolve external references, and the current
--   signature is used to resolve local references.
expandType :: SigEnv -> Signature -> Type -> Result Type
expandType _ _ TBool = return TBool
expandType _ _ TInt  = return TInt
expandType ext sig (arg :-> res) = do
    arg' <- expandType ext sig arg
    res' <- expandType ext sig res
    return (arg' :-> res')
expandType _ sig this@(TRef x)
    | Just t <- sigGetType x sig = return t
    | otherwise = notFound (InType this)
expandType ext _ this@(TExt m x)
    | Just t <- extGetMod m ext >>= sigGetType x = return t
    | otherwise = notFound (InType this)

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

-- | Typing relation for expressions. The typing context is a signature
--   environment for resolving external references, and a signature that
--   represents the local context for this expression.
typeExpr :: SigEnv -> Signature -> Expr -> Result Type

typeExpr _ _ (LitB _) = return TBool
typeExpr _ _ (LitI _) = return TInt

typeExpr ext sig this@(P1 o e) = do
    t <- typeExpr ext sig e
    case typeP1 o t of
      Just t -> return t
      _ -> mismatch this [(e,t)]

typeExpr ext sig this@(P2 o l r) = do
    lt <- typeExpr ext sig l
    rt <- typeExpr ext sig r
    case typeP2 o lt rt of
      Just t -> return t
      _ -> mismatch this [(l,lt),(r,rt)]

typeExpr ext sig this@(If c t e) = do
    ct <- typeExpr ext sig c
    tt <- typeExpr ext sig t
    et <- typeExpr ext sig e
    case ct of
      TBool | tt == et -> return tt
      _ -> mismatch this [(c,ct),(t,tt),(e,et)]

typeExpr ext sig (Abs x t e) = do
    t' <- expandType ext sig t
    res <- typeExpr ext (sigAddVal x t' sig) e
    return (t :-> res)

typeExpr ext sig this@(App l r) = do
    lt <- typeExpr ext sig l
    rt <- typeExpr ext sig r
    case lt of
      arg :-> res | rt == arg -> return res
      _ -> mismatch this [(l,lt),(r,rt)]

typeExpr ext sig this@(Fix e) = do
    t <- typeExpr ext sig e
    case t of
      arg :-> res | arg == res -> return res
      _ -> mismatch this [(e,t)]

typeExpr _ sig this@(Ref x)
    | Just t <- sigGetVal x sig = return t
    | otherwise = notFound (InExpr this)

typeExpr ext _ this@(Ext m x)
    | Just t <- extGetMod m ext >>= sigGetVal x = return t
    | otherwise = notFound (InExpr this)


--
-- * Typing the module system
--

-- ** Building and checking signatures

-- | Signature matching. A signature sub matches a signature sup if every
--   type or value declaration in sup is matched by a corresponding type or
--   value declaration in sub.
subSig :: Signature -> Signature -> Bool
subSig _ _ = True

-- | Load a declaration into a signature.
loadDecl :: SigEnv -> Signature -> Decl -> Result Signature
loadDecl _   sig this@(DType x Abstract)
    | sigHasType x sig = duplicate (InDecl this)
    | otherwise = return (sigAddType x (TRef x) sig)
loadDecl ext sig (DType x (Concrete t)) = do
    t' <- expandType ext sig t
    return (sigAddType x t' sig)
loadDecl ext sig (DVal x t) = do
    t' <- expandType ext sig t
    return (sigAddVal x t' sig)

-- | For a component binding in a module, produce the corresponding
--   declaration in the principal signature.
principalDecl :: SigEnv -> Signature -> Bind -> Result Decl
principalDecl ext sig (BType x t) = do
    t' <- expandType ext sig t
    return (DType x (Concrete t'))
principalDecl ext sig (BVal x e) = do
    t <- typeExpr ext sig e
    t' <- expandType ext sig t
    return (DVal x t')

-- | Build a principal signature from a list of bindings.
principalSig :: SigEnv -> Signature -> [Bind] -> Result Signature
principalSig _   sig []     = return sig
principalSig ext sig (b:bs) = do
    decl <- principalDecl ext sig b
    sig' <- loadDecl ext sig decl
    principalSig ext sig' bs


-- ** Typing relation

-- | Get the signature for a module expression.
typeMExpr :: SigEnv -> MExpr -> Result Signature
typeMExpr ext this@(MRef m)
    | Just sig <- extGetMod m ext = return sig
    | otherwise = notFound (InMod this)
typeMExpr ext (Mod bs) = principalSig ext envEmpty bs

-- | Get the signature for a signature expression.
typeSExpr :: SigEnv -> SExpr -> Result Signature
typeSExpr ext this@(SRef s)
    | Just sig <- extGetSig s ext = return sig
    | otherwise = notFound (InSig this)
typeSExpr ext (Sig ds) = foldM (loadDecl ext) envEmpty ds

-- | Type a top-level binding and add a corresponding entry to the signature
--   environment.
typeTop :: SigEnv -> Top -> Result SigEnv
typeTop ext (TMod m Nothing e) = do
    sig <- typeMExpr ext e
    return (extAddMod m sig ext)
typeTop ext (TMod m (Just se) me) = do
    sig <- typeMExpr ext me
    seal <- typeSExpr ext se
    -- check whether sig matches seal
    return (extAddMod m seal ext)
typeTop ext (TSig s e) = do
    sig <- typeSExpr ext e
    return (extAddSig s sig ext)

-- | Type check a program, producign the resulting signature environment.
typeProg :: Prog -> Result SigEnv
typeProg = foldM typeTop envEmpty
