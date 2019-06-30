module ProjectEuler.Problem34
  ( problem
  ) where


import Data.Char

import ProjectEuler.Types
import ProjectEuler.Problem24 (factorial)

problem :: Problem
problem = pureProblem 34 Solved result

verify :: Int -> Bool
verify x = sum (map (factorial . digitToInt) (show x)) == x

result :: Int
result = sum $ filter verify [3..99999]
