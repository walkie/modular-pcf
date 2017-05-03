module Language.ModPCF.TypeResult where

import Language.ModPCF.Syntax


--
-- * Typing results
--

-- | A typing result is either a type error or a successful result
--   (i.e. type or signature).
type Result a = Either TypeError a

-- | Captures the location and cause of a static type error.
data TypeError = TypeError Expr Cause
  deriving (Eq,Show)

-- | Causes of static type errors.
data Cause
   = Mismatch [(Expr,Type)]
   | NotFound
  deriving (Eq,Show)

-- | Was typing successful?
isSuccess :: Result Type -> Bool
isSuccess (Right _) = True
isSuccess _ = False

-- | Was there a type error?
isTypeError :: Result Type -> Bool
isTypeError (Left _) = True
isTypeError _ = False

-- | Was there a type mismatch?
isMismatch :: Result Type -> Bool
isMismatch (Left (TypeError _ (Mismatch _))) = True
isMismatch _ = False

-- | Was there an unbound variable error?
isNotFound :: Result Type -> Bool
isNotFound (Left (TypeError _ NotFound)) = True
isNotFound _ = False

-- | Throw a type mismatch error.
mismatch :: Expr -> [(Expr,Type)] -> Result Type
mismatch loc parts = Left (TypeError loc (Mismatch parts))

-- | Throw an unbound variable error.
notFound :: Expr -> Result Type
notFound loc = Left (TypeError loc NotFound)
