module Language.ModPCF.TypeResult where

import Language.ModPCF.Syntax


--
-- * Typing results
--

-- | A typing result is either a type error or a successful result
--   (i.e. type or signature).
type Result a = Either TypeError a

-- | Captures the location and cause of a static type error.
data TypeError
   = Mismatch Expr [(Expr,Type)]
   | NoSuchVar Expr
   | NoSuchTVar Type
   | DuplicateTVar Decl
  deriving (Eq,Show)

-- | Was typing successful?
isSuccess :: Result a -> Bool
isSuccess (Right _) = True
isSuccess _ = False

-- | Was there a type error?
isTypeError :: Result a -> Bool
isTypeError (Left _) = True
isTypeError _ = False

-- | Was there a type mismatch?
isMismatch :: Result a -> Bool
isMismatch (Left (Mismatch _ _)) = True
isMismatch _ = False

-- | Was there an unbound variable error?
isNoSuchVar :: Result a -> Bool
isNoSuchVar (Left (NoSuchVar _)) = True
isNoSuchVar _ = False

-- | Was there an unbound type variable error?
isNoSuchTVar :: Result a -> Bool
isNoSuchTVar (Left (NoSuchTVar _)) = True
isNoSuchTVar _ = False

-- | Was there a duplicate type variable error?
isDuplicateTVar :: Result a -> Bool
isDuplicateTVar (Left (DuplicateTVar _)) = True
isDuplicateTVar _ = False

-- | Throw a type mismatch error.
mismatch :: Expr -> [(Expr,Type)] -> Result a
mismatch loc parts = Left (Mismatch loc parts)

-- | Throw an unbound variable error.
noSuchVar :: Expr -> Result a
noSuchVar loc = Left (NoSuchVar loc)

-- | Throw an unbound type variable error.
noSuchTVar :: Type -> Result a
noSuchTVar loc = Left (NoSuchTVar loc)

-- | Throw a duplicate type variable error.
duplicateTVar :: Decl -> Result a
duplicateTVar loc = Left (DuplicateTVar loc)
