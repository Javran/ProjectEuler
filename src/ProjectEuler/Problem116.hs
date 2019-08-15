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
 -}

result = ()


