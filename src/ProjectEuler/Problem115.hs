module ProjectEuler.Problem115
  ( problem
  ) where

import Petbox

import ProjectEuler.Types
import ProjectEuler.Problem114 (fillCount)

problem :: Problem
problem = pureProblem 115 Solved result

{-
  Note that this problem is a generalization of Problem114,
  as shown below that the exact same function is shared between them.
 -}

result :: Int
result = firstSuchThat (\n -> fillCount 50 n > 1000000) [51..]
