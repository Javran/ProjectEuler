module ProjectEuler.Problem108
  ( problem
  ) where


import Control.Monad
import Data.Ratio
import ProjectEuler.Types

problem :: Problem
problem = Problem 108 Unsolved experiment

{-
  Given that:

  > 1/x + 1/y = 1/n
  > n = x*y / (x+y)

  Note that:
  - x and y are constrained so that one increases,
    the other will decrease.
  - when x = y, we have x = 2*n, this gives us an upper bound.
  - obviously `1 / (n-1) > 1 / n`, so this gives us a lower bound.

  so we have search space `n+1 <= x <= 2*n` that we can search for solutions,
  my plan is to just brute force that and see if we can find any clues on this.

 -}

search n = foldMap tryX [n+1 .. n+n] :: [(Int,Int)]
  where
    tryX x = do
      let r = (x - n) % (n * x)
      guard (numerator r == 1)
      pure (x, denominator r)

experiment =
  forM_ [1 :: Int ..100] $ \n -> do
    logT (n, length (search n))


