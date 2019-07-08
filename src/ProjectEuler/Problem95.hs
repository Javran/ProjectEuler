module ProjectEuler.Problem95
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.ArithmeticFunctions
import Control.Monad.State
import Data.Maybe

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

  1,000,000 numbers could be searchable - but it's still too slow.

  new idea: if aliquot sequence runs into prime, we should never include
  that as a candidate, that gives us only 13862 to search, but
  getting this list could be slow.

  explored: if we do only one step (with Maybe to eliminate candidates)
  at a time, it'll still take a while to get to the 7th interation.

  update: nub through IntSet eliminates more, but is still slow.

  TODO: memoization?

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

result = IS.size srcs
  where
    srcs = IS.fromDistinctAscList $ map fst pairs
    pairs =
      concatMap
        (\n -> let s = sumOfProperDivisors n in if s <= 1 then [] else [(n,s)])
        initSearchSpace
