module ProjectEuler.Problem116
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 116 Unsolved result

{-
  Idea: since colors are not allowed to mix,
  we can solve for each case independently.
  It's easy to see how many blocks can fit into the row,
  and for each fix number, n, of blocks,
  there are n+1 gaps where we can insert empty cells into them
  to reach the desired row length. I suspect Stirling numbers will
  somehow be involved.

  Let f(m,n) be the number of ways to fill a row measuring n,
  with tiles each measuring m.


  Let g(m,n,k) be the number of ways to fill a row measuring n,
  with k tiles each measuring m.

  Therefore:

  f(m,n) = sum of g(m,n,k) for k from 1 to floor(n/m)

  For g(m,n,k), there are k+1 gaps that we can fill empty cells
  to construct a row measuring n, and the number of empty cells
  needed is n - k*m.

  In other words, g(m,n,k) is also the way to put n-k*m empty cells
  into k+1 gaps while some gaps are allowed to be empty.
  (also all empty cells are identical, so permutation doesn't count)

  While Stirling numbers describes the way to put x elements into y non-empty subsets,
  we can workaround this restriction by fixing y from 1 to k+1 and sum the result up.
 -}

result = ()


