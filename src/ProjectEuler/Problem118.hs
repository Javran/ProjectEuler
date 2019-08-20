module ProjectEuler.Problem118
  ( problem
  ) where

import Petbox (pick)
import Math.NumberTheory.Primes.Testing (isPrime)
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 118 Unsolved result

{-
  Idea: this could be a set-cover problem:
  if we can list all primes whose every digit is unique,
  then we basically need to compute a set that has all digits
  being covered exactly once.
 -}

genPrimes :: Int -> [Int] -> [Int]
genPrimes acc candidates = do
  (d, candidates') <- pick candidates
  let acc' = acc*10 + d
  [acc' | isPrime (fromIntegral acc')] <> genPrimes acc' candidates'

result = length (genPrimes 0 [1..9]) -- 43089 primes that we need to consider.
