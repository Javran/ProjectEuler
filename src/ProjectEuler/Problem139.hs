module ProjectEuler.Problem139
  ( problem
  ) where

import Control.Monad

import ProjectEuler.Types
import Math.NumberTheory.Powers.Squares

problem :: Problem
problem = pureProblem 139 Unsolved result

{-
  Get some initial search going.

  for m > n > 0, we can generate pythagorean triples through:

  (NOTE: this generating method doesn't seem to play well with counting,
  we might as well scrap the plan.)

  a = m^2 - n^2
  b = 2*m*n
  c = m^2 + n^2

  For the tuple to be tile-able, we need: c `mod` abs(a-b) == 0.

  Perimeter:
  a + b + c = 2*m*(m+n) < 100,000,000.
 -}

-- NOTE: this one has the problem of missing counts even without gcd filter.
_result = length $ do
  m <- [1 .. 1000 :: Int]
  n <- filter (\n' -> gcd m n' == 1) [1 .. m-1]
  -- n <- [1 .. m-1]
  k <- takeWhile (\k' -> 2 * m * (m+n) * k' < 1000) [1..]
  let a = m * m - n * n
      b = 2 * m * n
      c = m * m + n * n
  guard $ c `mod` abs (a - b) == 0
  -- guard $ a + b + c < 1000
  pure (a * k, b*k, c*k)

-- hm, for perimeter < 1000, there suppose to be 99 triangles.
result = do
  c <- [1 :: Int ..1000]
  b <- [1..c-1]
  Just a <- [exactSquareRoot $ c*c - b*b]
  guard $ c `mod` abs (a - b) == 0
  guard $ a < b
  guard $ a + b + c < 1000
  pure (a,b,c)
