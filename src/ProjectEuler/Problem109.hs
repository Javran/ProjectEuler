module ProjectEuler.Problem109
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 109 Unsolved result

{-
  One of those annoying problems that deals with random rule.

  Dynamic programming could work, but find a way to encode
  those states could be trouble.

  What I have in mind for now is to try all
  moves in a sorted way (since permutations are considered
  the same unless it's the last move) and see how far
  can a straightforward search go.
 -}

result = ()


