module ProjectEuler.Problem94
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Powers.Squares

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 94 Solved result

{-
  Idea:

  Note that every isosceles triangle can be split
  into two right triangles whose shapes are the same.
  Given the context of this problem:

  - the slope of the right triangle (say, r) must be an integer.
  - the base of this isosceles triangle must be an integer.
  - and the length of the base muse either be r-1 or r+1.
  - in addition, the problem also require area to be an integer,
    which suggests height must be a rational number.

  At this point I suspect we need to generate Pythagorean triples
  and filter through it.

  Related link: https://en.wikipedia.org/wiki/Pythagorean_triple

  Let's use Euclid's formula:

  - a = k*(m*m - n*n)
  - b = k*(2*m*n)
  - c = k*(m*m + n*n)
  - and also require that:
    + c = 2a + 1
    + c = 2a - 1
    + c = 2b + 1
    + c = 2b - 1

  Note that in either way:

  - c - 2a = k*(3*n*n-m*m) = -1 or +1
  - c - 2b = k*(m*m+n*n-4*m*n) = -1 or +1

  k can only be 1 for this to work, so we can now just set k=1 to simplify it.

  Now the search space has been small enough for us to brute force (m,n)
  to get the final answer.

 -}

maxM :: Int
maxM = q
  where
    (q, _r) = integerSquareRootRem' $ maxPerimeters `quot` 3
    maxPerimeters = 1000000000

result :: Int
result = sum $ do
  m <- [maxM, maxM-1 .. 1]
  let case1 = do
        {-
          Assume c and 2a is off by 1,
          we can derive that: n^2 = (m^2 ± 1) / 3
         -}
        diff <- [-1, 1]
        (squared,0) <- [(m*m+diff) `quotRem` 3]
        Just n <- [exactSquareRoot squared]
        guard $ m > n && n > 0
        let c = m*m+n*n
            a = m*m-n*n
        pure $ c+c+a+a
      case2 = do
        {-
          Assume c and 2b is off by 1,
          (n-2m)^2 = (3m ± 1)^2
          therefore:
          n = 2m±sqrt((3m ± 1)^2)
         -}
        diff <- [-1, 1]
        let squared = 3*m*m + diff
        Just root <- [exactSquareRoot squared]
        n <- [2*m - root, 2*m + root]
        guard $ m > n && n > 0
        let c = m*m+n*n
            b = 2*m*n
        pure $ c+c+b+b
  case1 <> case2
