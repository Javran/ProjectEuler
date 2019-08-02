module ProjectEuler.Problem36
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 36 Solved result

result :: Int
result = sum
  [ x
  | x <- [1..1000000-1]
    -- the ordering is intentional: it's more likely and efficient
    -- to check for decimals before checking binaries.
  , x == numReverseInBase 10 x
  , x == numReverseInBase 2 x
  ]
