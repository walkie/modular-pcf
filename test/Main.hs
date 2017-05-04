module Main where

import Test.Tasty

import qualified Language.ModPCF.Test.Core as Core
import qualified Language.ModPCF.Test.EndToEnd as EndToEnd

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Core.tests
  , EndToEnd.tests
  ]
