{-# LANGUAGE NumericUnderscores #-}
module ProjectEuler.Problem132
  ( problem
  ) where

import Petbox
import Math.NumberTheory.Powers.Modular

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 132 Solved result

{-
  Apparently this doesn't sound right to me to
  actually carry out the division - let's see if we can
  go through primes and do divisibility tests.

  A repunit is R(k) = (10^k-1) / 9, and we want to find a list of
  primes (starting from smallest) divisible by it.

  Yes the following can be done in modular arithmetic,
  but we don't want that 9 to be absolved
  (as in scaling property a === b => a*k === b*k (mod p), which we are trying to avoid.)

  (10^(10^9) - 1) / 9 = k * p + 0 (k is an integer)
  > 10^(10^9) - 1 = k * p * 9
  > 10^(10^9) = 1 + k * 9*p

 -}

result :: Int
result =
  sum
  . take 40
  $ concatMap (\p -> [ p | 1 <- [powModInt 10 1_000_000_000 (9 * p)]]) primes
