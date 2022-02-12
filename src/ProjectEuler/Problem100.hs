module ProjectEuler.Problem100
  ( problem
  )
where

import Math.NumberTheory.Roots
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 100 Solved result

{-
  Let r be # of red discs, and b be # of blue discs,
  and we want: (b / (b+r)) * ((b - 1) / (b+r-1)) = 1 / 2

  Using some algebra manipulations we have:

  r^2 + (2*b-1)*r + (b-b^2) = 0

  since this is a quadratic equation,
  it's discriminant better be a square number:

  delta = (2*b-1)^2 - 4*1*(b-b^2) = 8*b^2 - 8*b + 1

  Now I'm interested to see if we can find some pattern that
  can lead me to a sequence of solutions of b (for delta to be a square number).

  > [b | b <- [1..], isSquare (8*b*b-8*b+1 :: Integer)]
  [1,3,15,85,493,2871,16731,97513,568345,3312555<takes forever>

  So this is good enough to give me https://oeis.org/A011900, which
  is how we arrive at current implementation of `bs` below.

 -}

rbPairs :: [(Integer, Integer)]
rbPairs = (\b -> (f b, b)) <$> bs
  where
    bs :: [Integer]
    bs = 1 : 3 : zipWith (\x y -> 6 * x - y -2) (tail bs) bs
    f b = (integerSquareRoot delta - (2 * b -1)) `div` 2
      where
        delta = 8 * b * b -8 * b + 1

result :: Integer
result = snd . head $ filter (\(r, b) -> r + b > (10 :: Integer) ^ (12 :: Int)) rbPairs
