module ProjectEuler.Problem131
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Primes

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 131 Unsolved result

{-
  No idea what I'm dealing with here, might begin with finding some
  of those primes and see what's going on.
 -}

-- This initial search points me to: https://oeis.org/A002407
result = reverse $ do
  m <- [10000 :: Int ,9999 ..1]
  let mCubed = m * m * m
  n <- [m-1,m-2..1]
  let nSq = n * n
      (q, r) = (mCubed - n * nSq) `quotRem` nSq
  guard $ r == 0
  Just _ <- pure $ isPrime q
  pure q
