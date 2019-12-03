{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem137
  ( problem
  ) where

import Data.Maybe
import Data.Word
import Math.NumberTheory.Powers.Squares

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 137 Solved result


{-
  I have no clue for this one, but 74049690 looks like a searchable number,
  which leads me to http://oeis.org/A081018 and I suspect this is the answer...

  Despite that let's do this properly: first let's just denote the series A(x).

    A(x) = x F_1 + x^2 F_2 + x^3 F_3 + ...
  x A(x) =         x^2 F_1 + x^3 F_2 + ...

  Therefore:

  A(x) + x A(x)
  = x F_1 + x^2 (F_2 + F_1) + x^3 (F_3 + F_2) + ...
  = x F_1 + x^2 F_3 + x^3 F_4 + ...
  = x F_2 + x^2 F_3 + x^3 F_4 + ... (since F_1 = F_2)
  = (A(x) - x F_1) / x

  from which we can conclude that: A(x) = -x / (x+x^2-1).

  Let A(x) = N, we get N x^2 + (N+1) x - N = 0, solution x will be rational
  if and only if the equation's discriminant is rational:

  b ^ 2 - 4ac = 5N^2 + 2N + 1, which must be a perfect square.
 -}

-- some early experiment - this does give us 74049690, as stated in the problem.

{-
  an early attempt to find the sequence, this results in:

  [(0,1),(2,5),(15,34),(104,233),(714,1597),(4895,10946),(33552,75025),(229970,514229),(1576239,3524578),(10803704,24157817),(74049690,165580141)]
 -}
_doSearch :: [(Integer, Integer)]
_doSearch =
  take 11
  $ mapMaybe
      (\(v,r) -> (v,) <$> exactSquareRoot r)
      [ (n, 1 + n*(2 + n*5)) | n <- [0..] ]

fibs :: [Word64]
fibs = 0:1:zipWith (+) (tail fibs) fibs

fibGoldenNugget :: Int -> Word64
fibGoldenNugget n = fibs !! (n+n) * fibs !! (n+n+1)

result :: Word64
result = fibGoldenNugget 15
