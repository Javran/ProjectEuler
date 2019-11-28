module ProjectEuler.Problem135
  ( problem
  ) where

import Data.Monoid
import Control.Monad
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.ArithmeticFunctions

import qualified Data.List.Match
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 135 Solved result

{-
  Let's get some symbol pushing going:

  (m+d)^2 - m^2 - (m-d)^2 = n > 0.

  w.l.o.g.: m > d > 0.
  this also implies that m > 1, we don't have a value for d otherwise.

  (m+d)^2 - m^2 - (m-d)^2
  = (m+d+m-d)(m+d-m+d) - m^2
  = 4 m d - m^2
  = (4 d - m) * m = n > 0

  so this gives us another constraint:

  4 d - m > 0 => m < 4 d,

  so for a fixed value n, m can be one of its divisor.

  d = (n + m^2) / (4 * m) must be an integer.

  If we further plug in d < m:

  (n + m^2) / 4 * m < m
  >  n < 3 * m^2

  This gives us a tighter range on m.
  Also note that since d can be expressed directly using only m and n,
  we only need to search for m and verify that d is valid.
  Let's try this idea out.

 -}

findSameDiffs :: Int -> [] (Int, Int)
findSameDiffs n = do
  let lo = integerSquareRoot' (n `quot` 3)
      (_, ds) = IS.split lo $ divisorsSmall n
  {-
    m > sqrt(n / 3) >= floor( sqrt(n / 3) ),
    therefore we can safely start with lo+1.
   -}
  m <- IS.toList ds
  let numer = n + m * m
      denom = 4 * m
  (d, 0) <- pure $ numer `quotRem` denom
  pure (m, d)

exactly10 :: [a] -> Bool
exactly10 = Data.List.Match.equalLength (replicate 10 ())

result :: Int
result =
  getSum $
    foldMap
      (\n -> if exactly10 . findSameDiffs $ n then 1 else 0)
      [1155 :: Int .. 1000000-1]

