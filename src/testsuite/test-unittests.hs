module Main where

import System.Exit (exitFailure)
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import GF.Utils
import GF.Command.PG_lttoolbox

tests :: [Test]
tests =
-- ************************************************************************* --
-- Tests for GF.Utils
-- ************************************************************************* --
  [ testGroup "GF.Utils" 
      [ testCase "replace" ("aaaaaa" @=? replace "b" "a" "ababab")
      , testCase "uniq" ("ab" @=? uniq "ababab")
      , testCase "split" (["houba","houba"] @=? split "houba houba" ' ') ]
-- ************************************************************************* --
-- Tests for GF.Commands.PG_lttoolbox
-- ************************************************************************* --
  , testGroup "GF.Commands.PG_lttoolbox"
      [ testCase "getPartOfSpeech \"paper_N\"" (Just "N" @=? getPartOfSpeech "paper_N")
      , testCase "getPartOfSpeech \"paper\"" (Nothing @=? getPartOfSpeech "paper")
      , testCase "getPartOfSpeech \"_paper\"" (Nothing @=? getPartOfSpeech "_paper")
      , testCase "getPartOfSpeech \"paper_\"" (Nothing @=? getPartOfSpeech "paper_") ]



  ]


main :: IO ()
main = defaultMain tests
 
