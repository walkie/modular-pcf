-- | This module represents "realized" signatures. That is, signatures as
--   maps from (type) variables to types, after expanding all type synonyms.
module Language.ModPCF.Signature where

import Language.ModPCF.Environment
import Language.ModPCF.Syntax


--
-- * Signatures
--

-- | A realized signature. Binds variable to types (value declarations) and
--   type variables to types (type declarations). All type synonyms are
--   expanded in the range of both maps. Abstract types are retained as type
--   variables.
type Signature = Env (Either Var TVar) Type

-- | Add a value declaration to a signature.
sigAddVal :: Var -> Type -> Signature -> Signature
sigAddVal = envAdd . Left

-- | Lookup a value declaration in a signature.
sigGetVal :: Var -> Signature -> Maybe Type
sigGetVal = envGet . Left

-- | Check for the existence of a value declaration in a signature.
sigHasVal :: Var -> Signature -> Bool
sigHasVal = envHas . Left

-- | Add a type declaration to a signature.
sigAddType :: TVar -> Type -> Signature -> Signature
sigAddType = envAdd . Right

-- | Lookup a type declaration in a signature.
sigGetType :: TVar -> Signature -> Maybe Type
sigGetType = envGet . Right

-- | Check for the existence of a type declaration in a signature.
sigHasType :: TVar -> Signature -> Bool
sigHasType = envHas . Right


--
-- * Signature environments
--

-- | The signature environment facilitates external references by binding
--   signature names to signatures, and binding module names to the
--   signatures that represent the "type" of the corresponding module.
type SigEnv = Env (Either SVar MVar) Signature

-- | Bind a signature name to a signature.
extAddSig :: SVar -> Signature -> SigEnv -> SigEnv
extAddSig = envAdd . Left

-- | Lookup the signature associated with a signature name.
extGetSig :: SVar -> SigEnv -> Maybe Signature
extGetSig = envGet . Left

-- | Check whether a signature name is bound in the environment.
extHasSig :: SVar -> SigEnv -> Bool
extHasSig = envHas . Left

-- | Bind a module name to a signature.
extAddMod :: MVar -> Signature -> SigEnv -> SigEnv
extAddMod = envAdd . Right

-- | Lookup the signature associated with a module name.
extGetMod :: MVar -> SigEnv -> Maybe Signature
extGetMod = envGet . Right

-- | Check whether a module name is bound in the environment.
extHasMod :: MVar -> SigEnv -> Bool
extHasMod = envHas . Right
