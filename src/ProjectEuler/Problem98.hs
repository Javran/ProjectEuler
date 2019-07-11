{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem98
  ( problem
  ) where

import Data.List
import Data.Ord
import qualified Data.List.Ordered as LOrdered

import Math.NumberTheory.Powers.Squares

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import ProjectEuler.Types
import ProjectEuler.SolCommon

problem :: Problem
problem = pureProblemWithData "p098_words.txt" 98 Unsolved compute

-- | non-deterministically picking an element from the given list,
--   separating the selected element and all other remaining elements
--   the list order is preserved
--   e.g. pick [1,2,3] == [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"

parseWords :: T.Text -> [String]
parseWords raw = read $ "[" <> T.unpack raw <> "]"

{-
  Group the list of words in terms of anagram,
  returns a list of pairs whose elements are (<set of char used>, <the group>).
  Also group that has less than 2 elements are dropped.
 -}
groupAnagrams :: [String] -> [(S.Set Char, [String])]
groupAnagrams =
    foldMap mkGroup
    . M.elems
    . M.fromListWith (<>)
    . fmap (\x -> (sort x, [x]))
  where
    mkGroup [] = []
    mkGroup [_] = []
    mkGroup xs@(h:_) = [(S.fromList h, xs)]

{-
  Now that we know in the worst case we'll have 9 characters
  that we need to assign each of them a value.

  so search space for trying all possible assignment will be:

  > product $ take 9 [9,8..]
  362880

  Of course first letter cannot be assigned 0, so this is an overestimation.

  Also there are only 42 groups after processing through "groupAnagrams",
  with many of them have only 4~6 letters to be assigned,
  I imagine brute forcing this won't take long.

  Also as a pontential optimization, we can choose to ignore Char
  and instead just work with sequence of numbers, but we are not going to do that
  until we have a working solution.

 -}

startSearch (cSet, ws) = search M.empty (S.toList cSet) [0..9] ws

search :: M.Map Char Int -> [] Char -> [] Int -> [String] -> [(String,String)]
search assigns remainedChars remainedDigits curWords
  | null curWords = []
  | null remainedChars =
      let validWords = filter validate curWords
          validate = isSquare' @Int . digitsToInt . fmap (assigns M.!)
      -- TODO: verify that assign actually gives square.
      in [(a,b) | a <- validWords, b <- validWords, a < b]
  | null remainedDigits = []
  | otherwise = do
      let (rc:remainedChars') = remainedChars
      (d,remainedDigits') <- pick remainedDigits
      let assigns' = M.insert rc d assigns
          curWords' = filter noLeadingZero curWords
          noLeadingZero [] = True
          noLeadingZero (h:_) = M.lookup h assigns' /= Just 0
      search (M.insert rc d assigns) remainedChars' remainedDigits' curWords'

compute = show . LOrdered.nubSort . concatMap startSearch . groupAnagrams . parseWords
