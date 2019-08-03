module ProjectEuler.Problem110
  ( problem
  ) where

import ProjectEuler.Types

{-
  This is basically the more difficult version of Problem108,
  my plan is to revisit that problem first and come back
  with perhaps some more insights.

  By the same insight we get from Problem108,

  we know `product (take 14 primes) = 13082761331670030`
  is a solution, but now problem lies in how to narrow this down:
  so far we are trying to reach 4000000 * 2 through powers of 3,
  but actually we should take product of numbers of odd numbers greater than 1,
  [3,5,7...] and see which of those are minimal.

 -}

problem :: Problem
problem = pureProblem 110 Unsolved result

result = ()


