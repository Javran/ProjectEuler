module ProjectEuler.Problem118
  ( problem
  ) where

import Petbox (pick, intToDigits)
import Math.NumberTheory.Primes.Testing (isPrime)
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import Data.Monoid
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 118 Unsolved result

{-
  Idea: this could be a set-cover problem:
  if we can list all primes whose every digit is unique,
  then we basically need to compute a set that has all digits
  being covered exactly once.

  one idea worth noting: if number a,b,c,... all has the same set of digits,
  we don't actually need to count them one-by-one.
  so if we organize these primes by the set of digits they have,
  we can save quite a bit of computation.

 -}

-- 43089 primes that we need to consider.
genPrimes :: Int -> [Int] -> [Int]
genPrimes acc candidates = do
  (d, candidates') <- pick candidates
  let acc' = acc*10 + d
  [acc' | isPrime (fromIntegral acc')] <> genPrimes acc' candidates'

result =
  -- now we only have 308 elements to do set-cover.
  M.size
  . M.fromListWith (<>)
  $ (\n -> (IS.fromList $ intToDigits n, 1 :: Sum Int)) <$> genPrimes 0 [1..9]
