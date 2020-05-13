module ProjectEuler.Problem124
  ( problem
  ) where

import Math.NumberTheory.Primes

import qualified Data.List.Ordered as LOrdered
import qualified Data.IntMap.Strict as IM
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 124 Solved result

{-
  Idea:

  Originally I want to generate radicals first them
  for each radical generate the set of numbers.

  But actually what's more efficient is to just
  compute factorisation for each numbers in range and group them together
  by radical number,

  This way we can simply toAscList and concatenate the resulting list
  to get the sorted version, and index into it.

  And since we are using a lazy language, one benefit is that a partial
  concatenation will save us some unnecessary work because we actually only
  visit about ~10% of the total elements (since list is linear) for the
  final indexing.

 -}

maxN :: Int
maxN = 100000

calcRadical :: Int -> Int
calcRadical x = product (fromIntegral . unPrime . fst <$> factorise x)

result :: Int
result =
    (!! 9999)
    . foldMap snd
    . IM.toAscList
    $ IM.fromListWith LOrdered.union (toPair <$> [1..maxN])
  where
    toPair :: Int -> (Int, [Int])
    toPair x = (calcRadical x, [x])
