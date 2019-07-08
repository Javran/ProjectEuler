module ProjectEuler.Problem95
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.ArithmeticFunctions

import qualified Data.List.Ordered as LOrdered
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 95 Unsolved result

{-

  Some ideas:
  - https://en.wikipedia.org/wiki/Sociable_number
  - https://en.wikipedia.org/wiki/Aliquot_sequence

  Since we want to find chains of values formed by Aliquot sequence,
  we can first compute (x, sumOfProperDivisors x) for all values in the search space,
  then we restrict keys by collecting all values of `sumOfProperDivisors` (cutting),
  and doing this repeatly will eventually get us to a point
  that all remaining values of this map is in some cycle and no more cutting
  can be made.

  Fortunately with 20 seconds, only 117 pairs are remaining,
  which is far more managable to search through.

 -}

maxN :: Int
maxN = 1000000

initSearchSpace :: [Int]
initSearchSpace = [1..maxN] `LOrdered.minus` primes

sumOfProperDivisors :: Int -> Int
sumOfProperDivisors n
  | n <= 1 = 0
  | otherwise =
      IS.foldl' (+) 0 (divisorsSmall n) - n

cut :: IM.IntMap Int -> IM.IntMap Int
cut m = IM.restrictKeys m vals
  where
    vals = IS.fromList $ IM.elems m

cutClear :: IM.IntMap Int -> IM.IntMap Int
cutClear m =
  if IM.size m == IM.size m'
    then m'
    else cutClear m'
  where
    m' = cut m

result = show loopMap
  where
    loopMap = cutClear $ IM.fromDistinctAscList pairs
    pairs =
      concatMap
        (\n -> let s = sumOfProperDivisors n in if s <= 1 then [] else [(n,s)])
        initSearchSpace
