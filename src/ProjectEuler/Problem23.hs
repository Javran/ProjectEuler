module ProjectEuler.Problem23
  ( problem
  ) where

import Data.Monoid

import qualified Data.IntSet as IS
import qualified Math.NumberTheory.Primes as Primes

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 23 Solved result

maxAbun :: Int
maxAbun = 28123

-- see: http://mathschallenge.net/library/number/sum_of_divisors
divisorSum :: Int -> Int
divisorSum n = product $ dSumPrimePow <$> Primes.factorise n
  where
    dSumPrimePow (p,a) = (p'^(a+1) - 1) `quot` (p'-1)
      where
        p' = Primes.unPrime p

isAbundant :: Int -> Bool
isAbundant n = n < divisorSum n - n

result :: Int
result =
    getSum $ IS.foldr' (\i acc -> Sum i <> acc) 0 (searchSpace IS.\\ reachables)
  where
    searchSpace = IS.fromDistinctAscList [1..maxAbun]
    possibleAbuns = IS.filter isAbundant searchSpace
    ts = IS.toList possibleAbuns
    reachables =
      IS.fromList $ do
        x <- ts
        let piv = maxAbun - x
            (ySet,isMem,_) = IS.splitMember piv possibleAbuns
        y <- [piv | isMem] <> IS.toList ySet
        pure (x+y)
