module ProjectEuler.Problem97
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 97 Solved result

(^^^) :: Integer -> Int -> Integer
a ^^^ b = a ^ b

result :: Integer
result = (28433 * (2^^^7830457) + 1) `mod` (10^^^10)
