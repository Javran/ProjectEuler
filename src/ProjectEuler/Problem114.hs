module ProjectEuler.Problem114
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 114 Unsolved result

{-
  Idea: perhaps dynamic programming is possible on this one:
  Let f(i,j) be the ways to occupy range i to j (inclusive)
  with position i and position j taken as edge of a block.

  We can grow the length between i and j to eventually cover
  whole set. Also beware that not putting any block at all counts
  as a valid solution, so we'll need to have some special handlings.
 -}

result = ()


