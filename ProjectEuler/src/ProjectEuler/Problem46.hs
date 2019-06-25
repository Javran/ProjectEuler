module ProjectEuler.Problem46
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Squares

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 46 Solved result

odds :: [Int]
odds = iterate (+2) 3

oddCompositeNumbers :: [Int]
oddCompositeNumbers = LOrdered.minus' odds primes

writtenAsPrimeAnd2Sq :: Int -> Bool
writtenAsPrimeAnd2Sq n =
    any primeAnd2Sq rangedPrimes
  where
    primeAnd2Sq p = r == 0 && isSquare' q
      where
        (q, r) = (n - p) `quotRem` 2
    rangedPrimes = takeWhile (<= n) primes

result :: Int
result =
  head $ filter (not . writtenAsPrimeAnd2Sq) oddCompositeNumbers

