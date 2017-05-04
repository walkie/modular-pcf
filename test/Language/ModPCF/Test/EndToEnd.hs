module Language.ModPCF.Test.EndToEnd where

import Test.Tasty
import Test.Tasty.HUnit

import Language.ModPCF.Environment
import Language.ModPCF.Examples
import Language.ModPCF.Semantics
import Language.ModPCF.Syntax
import Language.ModPCF.TypeCheck
import Language.ModPCF.TypeResult


tests = testGroup "End-to-end tests" testGCD

testGCD = 
  [ testCase "type-check gcd" (Right TInt @=? typeProg gcdProg)
  , testCase "evaluate gcd" (I 21 @=? evalExpr envEmpty (linkProg gcdProg))
  ]
