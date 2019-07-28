module ProjectEuler.Problem108
  ( problem
  ) where

import Math.NumberTheory.ArithmeticFunctions
import Control.Monad
import Data.Ratio
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 108 Solved result

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


  Actually initial search gives exactly what we want: https://oeis.org/A018892.

 -}

_search :: Int -> [] (Int, Int)
_search n = foldMap tryX [n+1 .. n+n]
  where
    tryX x = do
      let r = (x - n) % (n * x)
      guard (numerator r == 1)
      pure (x, denominator r)

fast :: Int -> Int
fast n = (tau (n*n) + 1) `quot` 2

{-
  TODO: for now it's still quite slow, need optimization.

  update: I tried with multiples of 2*3*5*7*11*13, which gives a faster solution.

  the idea is that we want to maximize tau.
 -}
-- TODO: cleanup

result :: Int
result =
  {-
    Since the search space is [n+1 .. n+n], we should at least have
    1000 candidates to check
   -}
  head
  . dropWhile (\n -> fast n < 1000)
  $ let t = 2*3*5*7*11*13 in iterate (+ t) t


