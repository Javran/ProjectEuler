module ProjectEuler.Problem28
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 28 Solved result

f :: Int -> Int
f x = 1 + 10 * x*x + ((16*x*x*x+26*x)`div`3)

result :: Int
result = f 500

