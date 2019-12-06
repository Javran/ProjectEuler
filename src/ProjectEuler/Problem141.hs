module ProjectEuler.Problem141
  ( problem
  ) where

import Math.NumberTheory.Powers.Squares

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
  - as r,d,q form a geometric sequence, we might as well give common ratio a name: c.
    + r < d < q => c > 1
    + c needs to be a rational since r, d, q are all integers.
    + therefore we can let a,b positive integers s.t. c = a / b (further, gcd(a,b) = 1 and a > b)
    + d = r * a / b

  n = d^3 / r + r
    = (r*a/b)^3 / r + r
    = (r^2 a^3) / b^3 + r (hm ... goes nowhere.)

  q = r * c^2 = (r a^2) / (b^2). since gcd(a,b) = 1, we must have r === 0 (mod b^2) for q to be an integer.
  let e be an integer s.t. r = e * b^2:

  r = e * b^2
  d = e * b^2 * a / b = e * a * b
  q = e * b^2 * a^2 / b^2 = e * a * a
  n = q * d + r = e^2 a^3 b + e b^2, which must also be a perfect square.

 -}

result = sum $ fmap fst xs
  where
    xs = do
      a <- [1 .. 10000 :: Integer]
      -- let aCube = a * a * a
      b <- filter (\b' -> gcd a b' == 1)[1.. a-1]
      let u = a * a * a * b
          v = b * b
      e <- takeWhile (\e' -> e' * e' * u + e' * v < 10^12) [1..]
      let n = e * e * u + e * v
      Just _ <- pure $ exactSquareRoot n
      let r = e * b * b
          d = e * a * b
          q = e * a * a
      pure (n, (r,d,q))

