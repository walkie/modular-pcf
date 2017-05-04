-- | This module defines the syntax of the Modular PCF language.
module Language.ModPCF.Syntax where

import Data.String (IsString)


--
-- * Names
--

-- $ Since there are several types of names in our language, we declare a
--   newtype for each. This helps ensure that we lookup the right variables
--   in the right environments. The OverloadedStrings extension makes all of
--   these newtypes relatively painless since we can create new variables of
--   any type with simple string literals.

-- | A name is a string.
type Name = String

-- | (Expression) variable names.
newtype Var = Var Name
  deriving (Eq,Ord,Show,IsString)

-- | Type variable names.
newtype TVar = TVar Name
  deriving (Eq,Ord,Show,IsString)

-- | Signature name.
newtype SVar = SVar Name
  deriving (Eq,Ord,Show,IsString)

-- | Module name.
newtype MVar = MVar Name
  deriving (Eq,Ord,Show,IsString)


--
-- * Core language
--

-- $ A simple functional core language. This is a variant of simply typed PCF.
--   Specifically, it includes integer and boolean literals and operations,
--   conditionals, lambda calculus, and a fixpoint operator.


-- ** Abstract syntax

-- | Basic types. Note that although the module system adds type variables,
--   there is no type quantification so the type system is not polymorphic.
--   That is, type variables are just abbreviations. The mechanism for
--   defining new type abbreviations is confined to the module system and
--   not accessible from the core language.
data Type
   = TBool           -- ^ boolean type
   | TInt            -- ^ integer type
   | Type :-> Type   -- ^ function type
   -- Type variables are an extension to support the module system
   | TRef TVar       -- ^ type variable
   | TExt MVar TVar  -- ^ external type variable
  deriving (Eq,Show)

infixr 9 :->

-- | Unary operations.
data Op1 = Not | Neg
  deriving (Eq,Show)

-- | Binary operations.
data Op2 = And | Or | Add | Mul | LTE
  deriving (Eq,Show)

-- | Expressions. The language is a variant of simply typed PCF. It consists
--   of primitive integers and booleans, conditionals, lambda calculus, and a
--   fixpoint operator.
data Expr
   = LitB Bool            -- ^ boolean literal
   | LitI Int             -- ^ integer literal
   | P1   Op1  Expr       -- ^ primitive unary operation
   | P2   Op2  Expr Expr  -- ^ primitive binary operation
   | If   Expr Expr Expr  -- ^ conditional expression
   | Abs  Var  Type Expr  -- ^ abstraction
   | App  Expr Expr       -- ^ application
   | Fix  Expr            -- ^ fixpoint operation
   | Ref  Var             -- ^ variable reference
   -- External references are an extension to support the module system
   | Ext  MVar Var        -- ^ external reference
  deriving (Eq,Show)


-- ** Syntactic sugar

-- | A parameter is a variable paired with its type.
type Param = (Var,Type)

-- | A boolean parameter.
bool :: Var -> Param
bool x = (x,TBool)

-- | An integer parameter.
int :: Var -> Param
int x = (x,TInt)

-- | Define a function that takes one argument as a parameter.
abs1 (x,t) e = Abs x t e

-- | Define a function that takes two arguments.
abs2 :: Param -> Param -> Expr -> Expr
abs2 (x1,t1) (x2,t2) e = Abs x1 t1 (Abs x2 t2 e)

-- | Define a function that takes three arguments.
abs3 :: Param -> Param -> Param -> Expr -> Expr
abs3 (x1,t1) (x2,t2) (x3,t3) e = Abs x1 t1 (Abs x2 t2 (Abs x3 t3 e))

-- | Apply a function to two arguments.
app2 :: Expr -> Expr -> Expr -> Expr
app2 f a b = App (App f a) b

-- | Apply a function to three arguments.
app3 :: Expr -> Expr -> Expr -> Expr -> Expr
app3 f a b c = App (App (App f a) b) c


--
-- * Module system
--

-- $ An ML-style module system for our core language.


-- ** Top-level programs

-- | A program is a sequence of top-level module and signature bindings,
--   plus an expression that represents the entry point (i.e. "main").
data Prog = Prog [Top] Expr
  deriving (Eq,Show)

-- | Top-level bindings of module and signature names.
data Top
   = TMod MVar (Maybe SExpr) MExpr
   | TSig SVar SExpr
  deriving (Eq,Show)


-- ** Signatures

-- | A type declaration is either abstract or concrete.
data TypeDecl
   = Abstract
   | Concrete Type
  deriving (Eq,Show)

-- | Component type and value declarations of a signature.
data Decl
   = DType TVar TypeDecl
   | DVal Var Type
  deriving (Eq,Show)

-- | Signature expressions in top-level bindings.
data SExpr
   = SRef SVar   -- ^ signature reference
   | Sig [Decl]  -- ^ new signature
  deriving (Eq,Show)


-- ** Modules

-- | Component type and value bindings of a module.
data Bind
   = BType TVar Type
   | BVal Var Expr
  deriving (Eq,Show)

-- | Module expressions in top-level bindings.
data MExpr
   = MRef MVar   -- ^ module reference
   | Mod [Bind]  -- ^ new module
  deriving (Eq,Show)
