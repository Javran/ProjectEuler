module ProjectEuler.Problem95
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.ArithmeticFunctions

import Debug.Trace

import qualified Data.List.Ordered as LOrdered
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 95 Unsolved result

{-

  Some ideas:
  - https://en.wikipedia.org/wiki/Sociable_number
  - https://en.wikipedia.org/wiki/Aliquot_sequence

  1,000,000 numbers could be searchable.

 -}

maxN :: Int
maxN = 1000000

initSearchSpace :: IS.IntSet
initSearchSpace = IS.fromDistinctAscList $  [1..maxN] `LOrdered.minus` primes

aliquotSet :: Int -> IS.IntSet
aliquotSet = aliquotSet' IS.empty

aliquotSet' :: IS.IntSet -> Int -> IS.IntSet
aliquotSet' s 1 = IS.insert 1 s
aliquotSet' s n =
    if IS.member n s || n > maxN
      then s
      else aliquotSet' (IS.insert n s) next
  where
    next = (sum . IS.toList $ divisorsSmall n) - n

search :: IS.IntSet -> [IS.IntSet] -> [IS.IntSet]
search searchSpace groups = case IS.maxView searchSpace of
  Nothing -> groups
  Just (x, _) ->
    let aliquotX = aliquotSet x
        searchSpace' = searchSpace `IS.difference` aliquotX
    in search searchSpace' (mergeIntoGroups groups aliquotX)

mergeIntoGroups :: [IS.IntSet] -> IS.IntSet -> [IS.IntSet]
mergeIntoGroups [] s = [s]
mergeIntoGroups (x:xs) s =
  if IS.null (IS.intersection x s)
    then x : mergeIntoGroups xs s
    else IS.union x s : xs

result = length $ search initSearchSpace []
