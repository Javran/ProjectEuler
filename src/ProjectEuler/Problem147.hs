module ProjectEuler.Problem147
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 147 Unsolved result

{-
  Idea: I guess this is a dynamic programming problem.
  but I expect this one to be somewhat difficult.

  Let's say a rectangle is "tightly situated" in a cross-hatched grid,
  if that rectangle has all its edges (or corners, if tilted) touching
  the edge of that grid. (well I know this definition isn't perfect for tilted
  rectangles, but for now let's just move on)

  If we define f(m,n) to be the number rectangles that can be tightly situated
  inside a m x n grid (m are columns and n rows).

  few properties to begin with:

  - f(1,1) = 1
  - f(m,n) = f(n,m)

  I imagine if we can find a way to get f (m+1) n from f c r  (1 <= c <= m, 1 <= r <= n),
  we will be up to a good start.

 -}

result = ()
