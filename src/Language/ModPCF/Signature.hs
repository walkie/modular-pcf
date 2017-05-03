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
