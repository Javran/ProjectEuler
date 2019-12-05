module ProjectEuler.Problem140
  ( problem
  ) where

import Data.List

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 140 Solved result

{-
  Idea: This one is very similar to Problem137, let's do the same thing.

    A(x) = x G_1 + x^2 G_2 + x^3 G_3 + ...
  x A(x) =         x^2 G_1 + x^3 G_2 + ...

  Therefore:

  (1+x) A(x) = x G_1 + (x^2 G_3 + x^3 G_4 + x^4 G_5 + ...)
             = x * 1 + (A(x) - x G_1 - x^2 G_2) / x
             = x + (A(x) - x - 4 x^2) / x

  Let t = A(x), we'll have:

  t = (3x^2 + x) / (1 - x - x^2)

  or, equivalently:

  (t+3) x^2 + (t+1) x - t = 0

  x to be rational, its discriminant must be a perfect square:

  b^2 - 4 a c = 5t^2 + 14 t + 1 = z^2

  => 5t^2 + 14 t + 1 - z^2 = 0.

  This can be solved by https://www.alpertron.com.ar/QUAD.HTM:

  - (t,z) = (2,-7), (0,-1), (0,1), (-4,5), (-3,2), (-3,-2)
  - and:
    + t_{n+1} = -9*t_n - 4*z_n - 14
    + z_{n+1} = -20*t_n - 9*z_n - 28
  - and:
    + t_{n+1} = -9*t_n + 4*z_n - 14
    + z_{n+1} = 20*t_n - 9*z_n + 28

  As there are many families of solutions to the equation,
  we can simply generate all of them (it so happens that they are all in ascending order
  if we ignore all negative terms) lazily, and take first 30 to form the solution.

 -}

result :: Int
result = sum . take 30 . foldl1 LOrdered.union $ do
    sol <- initSols
    next <- [next0, next1]
    let go s = let s'@(t',_) = next s in (t', s')
    pure $ filter (> 0) $ unfoldr (Just . go) sol
  where
    initSols = [(2,-7), (0,-1), (0,1), (-4,5), (-3,2), (-3,-2)]
    next0 (t,z) = (-9*t - 4*z - 14, -20*t - 9*z - 28)
    next1 (t,z) = (-9*t + 4*z - 14, 20*t - 9*z + 28)
