{-# LANGUAGE TypeApplications #-}

module ProjectEuler.Problem98
  ( problem
  )
where

import Control.Monad
import Data.List
import qualified Data.List.Ordered as LOrdered
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Math.NumberTheory.Roots
import Petbox
import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p098_words.txt" 98 Solved compute

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
    mkGroup xs@(h : _) = [(S.fromList h, xs)]

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

  (TODO) Also as a pontential optimization, we can choose to ignore Char
  and instead just work with sequence of numbers, but we are not going to do that
  until we have a working solution.

 -}

startSearch :: (S.Set Char, [String]) -> [((String, Int), (String, Int))]
startSearch (cSet, ws) = search M.empty (S.toList cSet) [0 .. 9] ws

search :: M.Map Char Int -> [] Char -> [] Int -> [String] -> [((String, Int), (String, Int))]
search assigns remainedChars remainedDigits curWords
  | null curWords = []
  | null remainedChars =
    let validWords = concatMap validate curWords
        validate w = do
          let val = digitsToInt . fmap (assigns M.!) $ w
          guard (isSquare @Int val)
          pure (w, val)
     in [(a, b) | a@(wa, _) <- validWords, b@(wb, _) <- validWords, wa < wb]
  | null remainedDigits = []
  | otherwise = do
    let (rc : remainedChars') = remainedChars
    (d, remainedDigits') <- pick remainedDigits
    let assigns' = M.insert rc d assigns
        curWords' = filter noLeadingZero curWords
        noLeadingZero [] = True
        noLeadingZero (h : _) = M.lookup h assigns' /= Just 0
    search (M.insert rc d assigns) remainedChars' remainedDigits' curWords'

compute :: T.Text -> Int
compute =
  maximum
    . concatMap (\((_, a), (_, b)) -> [a, b])
    . LOrdered.nubSort
    . concatMap startSearch
    . groupAnagrams
    . parseWords
