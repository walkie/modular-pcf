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
-- * Signature and module examples
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

-- | A signature defining an abstract data type for pairs of integers.
pairSig = Sig
   [ DType "Pair" Abstract
   , DVal "pair" (TInt :-> TInt :-> TRef "Pair")
   , DVal "fst"  (TRef "Pair" :-> TInt)
   , DVal "snd"  (TRef "Pair" :-> TInt)
   ]

-- | An implementation of the integer pair ADT based on Church encodings.
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
    $ App (Ref "p") (abs2 (int "x") (int "y") (Ref "x"))
  , BVal "snd"
    $ Abs "p" (TRef "Pair")
    $ App (Ref "p") (abs2 (int "x") (int "y") (Ref "y"))
  ]

-- | A signature that declares a greatest common divisor function. Assumes
--   that an implementation of the Pair signature is in scope as P.
gcdSig = Sig [ DVal "gcd" (TExt "P" "Pair" :-> TInt) ]

-- | A module that implements gcd using Euclid's algorithm, assumes that a
--   Pair and Int module are in scope as P and I.
--   @
--   fix(λf.λp. let a = fst p in
--              let b = snd p in
--              if a == b then p
--              else f (if a ≤ b then (a,b-a) else (a-b,b))
--   @
euclidMod = Mod
  [ BVal "euclid"
    $ Fix $ Abs "f" (TExt "P" "Pair" :-> TExt "P" "Pair")
    $ Abs "p" (TExt "P" "Pair")
    $ Let "a" (App (Ext "P" "fst") (Ref "p"))
    $ Let "b" (App (Ext "P" "snd") (Ref "p"))
    $ If (app2 (Ext "I" "equal") (Ref "a") (Ref "b"))
         (Ref "p")
         (App (Ref "f")
              (If (P2 LTE (Ref "a") (Ref "b"))
                  (app2 (Ext "P" "pair")
                        (Ref "a")
                        (app2 (Ext "I" "minus") (Ref "b") (Ref "a")))
                  (app2 (Ext "P" "pair")
                        (app2 (Ext "I" "minus") (Ref "a") (Ref "b"))
                        (Ref "b"))))
  , BVal "gcd"
    $ Abs "p" (TExt "P" "Pair")
    $ App (Ext "P" "fst") (App (Ref "euclid") (Ref "p"))
  ]

-- | Putting it all together.
gcdProg :: Prog
gcdProg = Prog
  [ TMod "I" Nothing intMod
  , TSig "PAIR" pairSig
  , TMod "P" (Just (SRef "PAIR")) pairMod
  , TSig "GCD" gcdSig
  , TMod "G" (Just (SRef "GCD")) euclidMod
  ]
  $ App (Ext "G" "gcd") (app2 (Ext "P" "pair") (LitI 1071) (LitI 462))
