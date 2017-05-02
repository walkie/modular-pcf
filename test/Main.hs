module Main where

import Test.Tasty

import qualified Language.ModPCF.Test.Core as Core

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Core.tests
  ]
