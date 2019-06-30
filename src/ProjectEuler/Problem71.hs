module ProjectEuler.Problem71
  ( problem
  ) where

import Data.Ratio

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 71 Solved result

{-
  see: http://en.wikipedia.org/wiki/Farey_sequence

  if two fractions are Farey neighbors, a/b < c/d,
  then we have:

    b*c - a*d = 1

  the converse is true: if b*c - a*d = 1, then
  a/b and c/d must be neighbors in Farey sequence of order max(b,d)

  therefore we can search all possible d <= 1,000,000 to find those pairs

  the closest neighbor must have the maximum value.

  in our problem, we know that: n/d < 3/7, so the target is:

  3d - 7n = 1, therefore:  n = (3*d-1) / 7, n is an integer

-}

solutions :: [Ratio Int]
solutions = do
  d <- [1..1000000]
  let n = 3*d-1
  (q,0) <- [n `quotRem` 7]
  pure (q % d)

result :: Int
result = numerator $ maximum solutions

