module ProjectEuler.Problem131
  ( problem
  ) where

import Data.Monoid
import Data.Maybe
import Control.Monad
import Math.NumberTheory.Primes

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 131 Solved result

{-
  No idea what I'm dealing with here, might begin with finding some
  of those primes and see what's going on.
 -}

-- This initial search points me to: https://oeis.org/A002407
_doSearch :: [Int]
_doSearch = reverse $ do
  m <- [10000, 9999 .. 1]
  let mCubed = m * m * m
  n <- [m-1, m-2 .. 1]
  let nSq = n * n
      (q, r) = (mCubed - n * nSq) `quotRem` nSq
  guard $ r == 0
  Just _ <- pure $ isPrime q
  pure q

-- TODO: perhaps put some explanation here as I don't feel it is actually thoughtfully solved.
result :: Int
result =
    getSum
    . foldMap (const 1)
    . takeWhile (< 1000000)
    . filter (isJust . isPrime)
    $ zipWith (-) (tail cubes) cubes
  where
    cubes :: [Int]
    cubes = (\x -> x ^ (3 :: Int)) <$> [1..]
