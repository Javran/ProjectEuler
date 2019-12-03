module ProjectEuler.Problem138
  ( problem
  ) where

import Data.Maybe
import Math.NumberTheory.Powers
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 138 Unsolved result

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
 -}

result = take 6 $ mapMaybe (\l -> do
                      z <- exactSquareRoot (5*l*l-1)
                      let r = z `mod` 5
                      guard $ r == 2 || r == 3
                      pure l
                  ) [1 :: Int ..]

