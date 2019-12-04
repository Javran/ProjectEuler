module ProjectEuler.Problem139
  ( problem
  ) where

import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 139 Unsolved result

{-
  Get some initial search going.

  for m > n > 0, we can generate pythagorean triples through:

  a = m^2 - n^2
  b = 2*m*n
  c = m^2 + n^2

  For the tuple to be tile-able, we need: c `mod` abs(a-b) == 0.

  Perimeter:
  a + b + c = 2*m*(m+n)*k < 100,000,000.

  since m > n:  2*m*(m+n)*k < 4 * m^2 * k

  this might overshoot but nonetheless gives us a rough bound to work with:

  we can guesstimate upperbound of m to be somewhere around sqrt(100,000,000 / 4) = 5000
 -}

result :: Int
result = length $ do
  -- was 5000, but then we have (m, n, k) = (5741,2378,1) ...
  m <- [1 .. 6000 :: Int]
  n <- [1 .. m-1]
  -- it is important that m and n are not both odd.
  guard $ gcd m n == 1 && (even m || even n)
  k <- takeWhile (\k' -> 2 * m * (m+n) * k' < 100000000) [1..]
  let a = m * m - n * n
      b = 2 * m * n
      c = m * m + n * n
  guard $ c `mod` abs (a - b) == 0
  pure (a*k, b*k, c*k)
