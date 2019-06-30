module ProjectEuler.Problem6
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 6 Solved result

result :: Int
result = sq (sum xs) - sum (map sq xs)
  where
    xs = [1..100]
    sq :: Int -> Int
    sq x = x * x

