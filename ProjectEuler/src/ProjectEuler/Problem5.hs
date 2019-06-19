module ProjectEuler.Problem5
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 5 Solved result

result :: Int
result = foldr lcm 1 [1..20]
