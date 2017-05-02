module Language.ModPCF.Test.Core where

import Test.Tasty
import Test.Tasty.HUnit

import Language.ModPCF.Environment
import Language.ModPCF.Examples
import Language.ModPCF.Semantics
import Language.ModPCF.Syntax
import Language.ModPCF.TypeCheck


tests = testGroup "Core language tests" $ concat
  [ typeEqual
  , evalEqual
  , typeFact
  , evalFact
  ]

equal_3_5 = app2 intEqual (LitI 3) (LitI 5)
equal_5_3 = app2 intEqual (LitI 5) (LitI 3)
equal_3_3 = app2 intEqual (LitI 3) (LitI 3)
equal_3_true = app2 intEqual (LitI 3) (LitB True)

typeEqual =
  [ testCase "typeEqual 1" (Right TBool @=? typeExpr envEmpty equal_3_5)
  , testCase "typeEqual 2" (Right TBool @=? typeExpr envEmpty equal_5_3)
  , testCase "typeEqual 3" (Right TBool @=? typeExpr envEmpty equal_3_3)
  , testCase "typeEqual 4" (assertBool "" (isMismatch (typeExpr envEmpty equal_3_true)))
  ]

evalEqual =
  [ testCase "evalEqual 1" (B False @=? evalExpr envEmpty equal_3_5)
  , testCase "evalEqual 2" (B False @=? evalExpr envEmpty equal_5_3)
  , testCase "evalEqual 3" (B True  @=? evalExpr envEmpty equal_3_3)
  ]

typeFact =
  [ testCase "typeFact" (Right (TInt :-> TInt) @=? typeExpr envEmpty factorial)
  ]

evalFact =
  [ testCase "evalFact 1" (I 120 @=? evalExpr envEmpty (App factorial (LitI 5)))
  , testCase "evalFact 2" (I 3628800 @=? evalExpr envEmpty (App factorial (LitI 10)))
  ]
