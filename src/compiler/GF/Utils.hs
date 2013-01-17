{-| This modules is there to regroup miscalennous useful functions
    that aren't available in the haskell base library
-}
module GF.Utils (replace,uniq,split) where

import qualified Data.Set as Set
import Data.List (isPrefixOf)

-- | Replace a sub-list with an other one.
-- this is a basic implementation of a string 
-- search and replace function
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace  _ _ [] = []
replace before after xs@(a:as) 
  | isPrefixOf before xs = after ++ replace before after (drop (length before) xs)
  | otherwise            = a : replace before after as


-- | it returns the input list without duplicates like the unix function
-- with the same name. But instead of removing only consecutive duplicates,
-- it remove all of them)
uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList


-- | Splits a list according to an item
split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = [[]]
split (c:cs) delim
      | c == delim = [] : rest
      | otherwise = (c : head rest) : tail rest
 where
   rest = split cs delim


