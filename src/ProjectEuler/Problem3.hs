module ProjectEuler.Problem3
  ( problem
  ) where

import ProjectEuler.Types
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Primes.Sieve

problem :: Problem
problem = pureProblem 3 Solved result

n :: Integer
n = 600851475143

result :: Integer
result =
    head . filter (\x -> n `rem` x == 0)
    . reverse
    . takeWhile (<= q)
    $ primes
  where
    q = integerSquareRoot' n
