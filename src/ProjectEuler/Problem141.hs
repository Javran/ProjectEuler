module ProjectEuler.Problem141
  ( problem
  ) where

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.ArithmeticFunctions

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 141 Unsolved result

{-
  Looks like this will be one of those difficult ones.

  Say n = q * d + r.

  - First, we don't want any of those to be zero,
    since we want q,d,r to form geometric sequence (not in that particular order).
    Therefore: q, d, r are all positive.
  - Also 0 < r < d.
  - note that swapping q and d makes no change to the equation n = q * d + r,
    we might as well make a fixed relation d < q w.l.o.g.
  - so we end up with 0 < r < d < q.

  r < d < q => r * q = d * d, q = d * d / r

  n = q * d + r = d^3 / r + r, which needs to be a perfect square.

  - Since r is an integer, so must be d^3 / r, which means d^3 === 0 (mod r)

 -}

result :: Integer
result = sum xs
  where
    xs = LOrdered.nubSort $ fmap fst $ do
      d <- [1 .. 1000000 :: Integer]
      let dCube = d * d * d
      r <- divisorsList dCube
      let n = (dCube `quot` r) + r
      True <- pure $ n < 10 ^12
      Just _ <- pure $ exactSquareRoot n
      (q, 0) <- pure $ (d * d) `quotRem` r
      True <- pure $ r < d && d < q
      pure (n, (r,d,q))

