module ProjectEuler.Problem53
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 53 Solved result

sel :: Integer -> Integer -> Integer
sel n r = product [n-r+1..n] `div` product [1..r]

result :: Int
result = length [ () | n <- [1..100], r <- [0..n], sel n r > 1000000 ]

