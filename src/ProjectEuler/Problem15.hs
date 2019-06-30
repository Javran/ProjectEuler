module ProjectEuler.Problem15
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 15 Solved result

-- https://oeis.org/A000984
result :: Integer
result = maximum [ fa (i+j) `div` (fa i * fa j)
                 | i<- [0..20]
                 , j<- [0..20]
                 ]
  where
    fa x = if x > 0 then x * fa (x-1)
                    else 1
