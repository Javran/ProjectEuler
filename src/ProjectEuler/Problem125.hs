module ProjectEuler.Problem125
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 125 Unsolved result

{-
  Idea: given n, we know that

  1^2 + 2^2 + ... n^2 = (n * (n+1) * (2n+1)) / 6

  using this, we can easily compute sum of squares
  for one number m to another one n.

  To find the upperbound:

  > let f n = (n * (n+1) * (2*n+1)) `div` 6
  > f 1
  1
  > f 2
  5
  > f 6
  91
  > head $ dropWhile ((< 100000000) . f) [1..]
  669

  Doesn't sound like a lot to just brute force.

 -}

result = ()


