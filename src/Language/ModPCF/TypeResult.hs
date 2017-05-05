module Language.ModPCF.TypeResult where

import Language.ModPCF.Syntax
import Language.ModPCF.Signature


--
-- * Typing results
--

-- | A typing result is either a type error or a successful result
--   (i.e. type or signature).
type Result a = Either TypeError a

-- | Captures the context and cause of a static type error.
data TypeError = TypeError Context Cause
  deriving (Eq,Show)

-- | The context that an error occurred in.
data Context
   = InExpr Expr
   | InType Type
   | InDecl Decl
   | InBind Bind
   | InMod  MExpr
   | InSig  SExpr
   | InTop  Top
  deriving (Eq,Show)

-- | What caused the error.
data Cause
   = Mismatch [(Expr,Type)]           -- ^ type mismatch
   | SigMismatch Signature Signature  -- ^ signature mismatch
   | NotFound                         -- ^ unbound name
   | Duplicate                        -- ^ duplicate name declaration
  deriving (Eq,Show)

-- | Was typing successful?
isSuccess :: Result a -> Bool
isSuccess (Right _) = True
isSuccess _ = False

-- | Was there a type error?
isError :: Result a -> Bool
isError (Left _) = True
isError _ = False

-- | Was there a type mismatch?
isMismatch :: Result a -> Bool
isMismatch (Left (TypeError _ (Mismatch _))) = True
isMismatch _ = False

-- | Was there an unbound name error?
isNotFound :: Result a -> Bool
isNotFound (Left (TypeError _ NotFound)) = True
isNotFound _ = False

-- | Was there a duplicate name declaration error?
isDuplicate :: Result a -> Bool
isDuplicate (Left (TypeError _ Duplicate)) = True
isDuplicate _ = False

-- | Throw a type mismatch error.
mismatch :: Expr -> [(Expr,Type)] -> Result a
mismatch loc parts = Left (TypeError (InExpr loc) (Mismatch parts))

-- | Throw a signature mismatch error.
sigMismatch :: Top -> Signature -> Signature -> Result a
sigMismatch loc sig seal = Left (TypeError (InTop loc) (SigMismatch sig seal))

-- | Throw an unbound name error.
notFound :: Context -> Result a
notFound loc = Left (TypeError loc NotFound)

-- | Throw a duplicate name declaration error.
duplicate :: Context -> Result a
duplicate loc = Left (TypeError loc Duplicate)
