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
    Therefore: q > 0 & d > 0 & r > 0
  - Also 0 < r < q

  So r < q is already in place, leaving d only 3 choices:
  - d < r < q, in this case we can verify whether d * q == r * r
  - r < d < q, verify that r * q == d * d
  - r < q < d, verify that r * d == q * q

  Perhaps explore a bit more with each cases?

  - case #1: d < r < q => r * r = d * q

    n = q * d + r = r * (r + 1) = x * x

    Given that n > 0, there is no solution for when x > 0.

  - case #2: r < d < q => r * q = d * d, q = d * d / r

    n = q * d + r = d^3 / r + r, which needs to be a perfect square.

    Since r is an integer, so must be d^3 / r, which means d^3 === 0 (mod r)

  - case #3: r < q < d => r * d = q * q. d = q * q / r

    n = q * d + r = q^3 / r + r, which needs to be a perfect square.

  Given that the analysis of case #2 and case #3 ends up in basically the same
  equation: n = a^3 / r + r, and the problem limits n < 10^12,
  it is probably sufficient to search inside a < 10^4 to find all the solutions.

  Update: I don't think we've got the right way to put an upperbound on things,
  given that r could be large.

  Update: got the right answer, but it's a guessed upperbound & slow.
  not as good as solved.

 -}

result :: Integer
result = sum xs
  where
    xs = LOrdered.nubSort $ fmap fst $ do
      a <- [1 .. 1000000 :: Integer]
      let aCube = a * a * a
      r <- divisorsList aCube
      let n = (aCube `quot` r) + r
      True <- pure $ n < 10 ^12
      Just _ <- pure $ exactSquareRoot n
      [ (n, (r,d,q))
        | let d = a, (q, 0) <- [(d * d) `quotRem` r]
        , r < d && d < q
        ] <> [
        (n, (r,q,d))
        | let q = a
        , (d, 0) <- [(q * q) `quotRem` r]
        , r < q && q < d
        ]
