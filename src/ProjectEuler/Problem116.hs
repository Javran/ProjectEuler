module ProjectEuler.Problem116
  ( problem
  ) where

import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 116 Solved result

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

  Not quite there: for Stirling numbers to work, every object needs to be
  distinct, which in our case is not the case.
 -}
f :: Int -> Int -> Integer
f m n = sum (g m n <$> [1 .. kMax])
  where
    kMax = n `quot` m

g :: Int -> Int -> Int -> Integer
g m n k = ballsInBins (n-k*m) (k+1)

{-
  sub-problem: put k identical balls into n bins, how many ways are there? (empty bins are allowed)

  to separate them into n bins, we need n-1 separator,
  and there are in total n-1+k elements we need to place, in which (n-1) of them are separators.
  therefore (n+k-1) choose (n-1)
 -}
ballsInBins :: Int -> Int -> Integer
ballsInBins k n = fromIntegral (n+k-1) `choose` fromIntegral (n-1)

{- TODO: cleanup -}

sol :: Int -> Integer
sol n = f 2 n + f 3 n + f 4 n

result :: Integer
result = sol 50

