module ProjectEuler.Problem147
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 147 Unsolved result

{-
  Idea: I guess this is a dynamic programming problem.
  but I expect this one to be somewhat difficult.

  Observation: we should be able to treat this as two separated counting problem:

  - putting rectangles into grids without cross-hatches.
  - putting rectangles into grids with only cross-hatches.

  So I previously want to see how far we can go with a concept of "tightly situated",
  turns out this does not lead us too far. This is simply because a "tightly situated"
  situation has, well, exactly one solution by definition. So instead we should really
  try counting with less constraints and see how it goes:

  If we define f(m,n) to be the number rectangles that can be situated
  inside a m x n grid (m are columns and n rows).

  few properties to begin with:

  - f(1,1) = 1
  - f(m,n) = f(n,m)

  I imagine if we can find a way to get f (m+1) n from f c r  (1 <= c <= m, 1 <= r <= n),
  we will be up to a good start.

 -}

result = ()
