module ProjectEuler.Problem10
  ( problem
  ) where

import ProjectEuler.Types
import Math.NumberTheory.Primes

problem :: Problem
problem = pureProblem 10 Solved result

result :: Integer
result = sum . takeWhile (<2000000) $ primes

