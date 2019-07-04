module ProjectEuler.Problem66
  ( problem
  ) where

import ProjectEuler.Types
import ProjectEuler.Problem64 (gen, getPeriod)

problem :: Problem
problem = pureProblem 66 Unsolved result

{-
  Some leads:
  - https://en.wikipedia.org/wiki/Pell's_equation
  - Problem64
 -}
result = (\z -> (z, take 10 . gen $ z)) <$> [2,3,5,6,7]
