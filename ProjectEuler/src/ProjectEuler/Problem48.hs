module ProjectEuler.Problem48
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 48 Solved result

type I = Integer

lim :: I -> I
lim = (`rem` 10000000000)

powerMod :: I -> I -> I
powerMod = powerModAux 1
  where
    -- (c * x^y) mod m is a constant
    powerModAux c x y
      | y == 0  = lim c
      | otherwise =
        if odd y
          then lim $ powerModAux (c*x) x (y-1)
          else lim $ powerModAux c (x*x) (y `quot` 2)

result :: I
result =
  foldl (\acc x -> lim (acc + powerMod x x)) 0 [1..1000]
