module ProjectEuler.Problem1 (problem) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 1 Solved result

result :: Int
result = sum [ x | x <- [1 .. 999] , x `rem` 3 == 0 || x `rem` 5 == 0 ]
