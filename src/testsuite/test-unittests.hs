module Main where

import System.Exit (exitFailure)
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import GF.Utils

tests :: [Test]
tests = [
-- ************************************************************************* --
-- Tests for GF.Utils
-- ************************************************************************* --
    testGroup "GF.Utils" 
      [ testCase "replace" ("aaaaaa" @=? replace "b" "a" "ababab")
      , testCase "uniq" ("ab" @=? uniq "ababab")
      , testCase "split" (["houba","houba"] @=? split "houba houba" ' ') ]
  ]


main :: IO ()
main = defaultMain tests
 
