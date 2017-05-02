module Language.ModPCF.Examples where

import Language.ModPCF.Syntax


--
-- * Core language examples
--

-- | A function that checks whether two integers are equal.
--   @
--   λxy. x ≤ y & y ≤ x
--   @
intEqual
  = abs2 (int "x") (int "y")
  $ P2 And (P2 LTE (Ref "x") (Ref "y"))
           (P2 LTE (Ref "y") (Ref "x"))

-- | The factorial function.
--   @
--   fix(λf.λn. if n ≤ 1 then 1 else n * f (n-1))
--   @
factorial 
  = Fix $ Abs "f" (TInt :-> TInt)
  $ abs1 (int "n")
  $ If (P2 LTE (Ref "n") (LitI 1))
       (LitI 1)
       (P2 Mul (Ref "n")
               (App (Ref "f") (P2 Add (Ref "n") (P1 Neg (LitI 1)))))


--
-- * Module examples
--

-- | A module with functions for working with integers.
intMod = Mod
  [ BVal "plus"
    $ abs2 (int "x") (int "y")
    $ P2 Add (Ref "x") (Ref "y")
  , BVal "times"
    $ abs2 (int "x") (int "y")
    $ P2 Mul (Ref "x") (Ref "y")
  , BVal "minus"
    $ abs2 (int "x") (int "y")
    $ P2 Add (Ref "x") (P1 Neg (Ref "y"))
  , BVal "equal"
    $ intEqual
  , BVal "min"
    $ abs2 (int "x") (int "y")
    $ If (P2 LTE
           (app2 (Ref "minus") (Ref "x") (Ref "y"))
           (LitI 0))
         (Ref "x")
         (Ref "y")
  , BVal "max"
    $ abs2 (int "x") (int "y")
    $ If (P2 LTE
           (app2 (Ref "minus") (Ref "x") (Ref "y"))
           (LitI 0))
         (Ref "y")
         (Ref "x")
  ]

-- | A module with functions for working with pairs of integers.
--   Implementation is based on Church encodings.
pairMod = Mod
  -- type
  [ BType "Pair" ((TInt :-> TInt :-> TInt) :-> TInt)
  -- constructor
  , BVal "pair"
    $ abs2 (int "x") (int "y") 
    $ Abs "sel" (TInt :-> TInt :-> TInt)
    $ app2 (Ref "sel") (Ref "x") (Ref "y")
  -- destructors
  , BVal "fst"
    $ Abs "p" (TRef "Pair")
    $ App (abs2 (int "x") (int "y") (Ref "x")) (Ref "p")
  , BVal "snd"
    $ Abs "p" (TRef "Pair")
    $ App (abs2 (int "x") (int "y") (Ref "x")) (Ref "p")
  ]
