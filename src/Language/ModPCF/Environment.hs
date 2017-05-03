module Language.ModPCF.Environment where

import Data.Map (Map)
import qualified Data.Map as M

import Language.ModPCF.Syntax


--
-- * Generic environments
--

-- | An environment maps variables to to some kind of value.
type Env var a = Map var a

-- | The empty environment.
envEmpty :: Env var a
envEmpty = M.empty

-- | Add a binding to the environment.
envAdd :: Ord var => var -> a -> Env var a -> Env var a
envAdd = M.insert

-- | Lookup a binding in the environment.
envGet :: Ord var => var -> Env var a -> Maybe a
envGet = M.lookup


--
-- * Signatures
--

-- | The signature environment associates signatures with module names.
type SigEnv = Map MVar Signature

-- | A realized signature. Binds variable to types (value declarations) and
--   type variables to types (type declarations). All type synonyms are
--   expanded in the range of both maps. Abstract types are retained as type
--   variables.
type Signature = (Map Var Type, Map TVar Type)

-- | Add a value declaration to a signature.
sigAddVal :: Var -> Type -> Signature -> Signature
sigAddVal x t (vs,ts) = (M.insert x t vs, ts)

-- | Add a type declaration to a signature.
sigAddType :: TVar -> Type -> Signature -> Signature
sigAddType x t (vs,ts) = (vs, M.insert x t ts)

-- | Lookup a value declaration in a signature.
sigGetVal :: Var -> Signature -> Maybe Type
sigGetVal x (vs,_) = M.lookup x vs

-- | Lookup a type declaration in a signature.
sigGetType :: TVar -> Signature -> Maybe Type
sigGetType x (_,ts) = M.lookup x ts

-- | Check for the existence of a value declaration in a signature.
sigHasVal :: Var -> Signature -> Bool
sigHasVal x (vs,_) = M.member x vs

-- | Check for the existence of a type declaration in a signature.
sigHasType :: TVar -> Signature -> Bool
sigHasType x (_,ts) = M.member x ts

-- | Load a declaration into a signature.
loadDecl :: SigEnv -> Signature -> Decl -> Maybe Signature
loadDecl _   sig (DType x Abstract) = do
    -- guard (isigHasType x sig)
    return (sigAddType x (TRef x) sig)
loadDecl ext sig (DType x (Concrete t)) = do
    t' <- expandType ext sig t
    return (sigAddType x t' sig)
loadDecl ext sig (DVal x t) = do
    t' <- expandType ext sig t
    return (sigAddVal x t' sig)

-- | Expand all type synonyms in a type.
expandType :: SigEnv -> Signature -> Type -> Maybe Type
expandType _ _ TBool = return TBool
expandType _ _ TInt  = return TInt
expandType ext sig (arg :-> res) = do
    arg' <- expandType ext sig arg
    res' <- expandType ext sig res
    return (arg' :-> res')
expandType _ sig (TRef x) =
    sigGetType x sig
expandType ext _ (TExt m x) = do
    sig <- M.lookup m ext
    sigGetType x sig
