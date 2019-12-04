module ProjectEuler.Problem138
  ( problem
  ) where

import Data.Maybe
import Math.NumberTheory.Powers
import Control.Monad
import Data.Word

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 138 Solved result

{-
  Idea:

  first we know that a triangle with sides being b/2, h, L
  should be a right triangle with side measuring L being hypotenuse.

  (b/2)^2 + h^2 = L^2
  > b^2 / 4 + (b^2 ±2b + 1) = L^2
  > (5/4) b^2 ±2b + (1-L^2) = 0

  this is a quadratic equation, computing discriminant:

  B^2 - 4 A C = (±2)^2 - 4 * 5/4 * (1-L^2) = 5L^2 - 1,

  for the equation to get an integer solution, the discriminant must be a perfect square.

  further, we have:

  b = (- B ± sqrt(5L^2 - 1) ) / (2 A)
    = (∓4 ± 2 sqrt(5L^2 - 1)) / 5
    = (2 sqrt(5L^2 - 1) ∓ 4) / 5) (since L > 1, we know 2 sqrt(5L^2 - 1) > 4 and we want b > 0)

  which needs to be an integer.

  (2 sqrt(5L^2 - 1) ∓ 4) === 0 (mod 5)
  > 2 sqrt(5L^2 - 1) === ± 4 (mod 5)
  > 3 * 2 sqrt(5L^2 - 1) === 3 * (± 4) (mod 5)
  > sqrt(5L^2 - 1) === 2 or 3 (mod 5)

  So to do this properly, we'll need to deal with some Pell's equations:

  (b/2)^2 + h^2 = L^2
  > 5 b^2 ± 8 b + 4 - 4 L^2 = 0

  Plug in this to: https://www.alpertron.com.ar/QUAD.HTM

  When d = 8: (5 x^2 - 4 y^2 + 8 x + 4 = 0)

  - x = 0, y = 1
  - solutions:
    + x_{n+1} = -9 x_n - 8 y_n - 8
    + y_{n+1} = -10 x_n - 9 y_n - 8
  - and also:
    + x_{n+1} = 9 x_n + 8 y_n - 8
    + y_{n+1} = 10 x_n - 9 y_n + 8

  When d = -8: (5 ⁢x^2 - 4 ⁢y^2 - 8 ⁢x + 4 = 0)

  - x = 0, y = -1
  - solutions:
    + x_{n+1} = -9 x_n + 8 y_n + 8
    + y_{n+1} = -10 x_n - 9 y_n + 8
  - and also:
    + x_{n+1} = - 9 x_n + 8 y_n + 8
    + y_{n+1} = 10 x_n - 9 y_n - 8

 -}

{-
  Initial search: [1,17,305,5473,98209,1762289,31622993],
  which leads me to: http://oeis.org/A007805
 -}
_doSearch :: [Int]
_doSearch =
  take 7
  $ mapMaybe
      (\l -> do
          z <- exactSquareRoot (5*l*l-1)
          let r = z `mod` 5
          guard $ r == 2 || r == 3
          pure l)
      [1..]

fibs :: [Word64]
fibs = 0:1:zipWith (+) (tail fibs) fibs

result :: Word64
result = sum $ f <$> [1..12]
  where
    f n = fibs !! (6*n+3) `quot` 2

{-
  The following solution is given by https://www.alpertron.com.ar/QUAD.HTM,
  should be the same solution.
 -}
_resultAlt :: Int
_resultAlt =
    sum . fmap (abs . snd)
    . take 12 . tail
    $ iterate next (0, 1)
  where
    next (x, y) = (-9 * x - 8 * y - 8, -10 * x  - 9 * y - 8)
