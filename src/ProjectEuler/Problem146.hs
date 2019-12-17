module ProjectEuler.Problem146
  ( problem
  ) where

import Data.Maybe
import Math.NumberTheory.Primes
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 146 Unsolved result

{-
  Idea: let's do it the stupid way, maybe there are some insights.
 -}

hasPrimePattern :: Integer -> Bool
hasPrimePattern n = isJust $ do
    mapM_ tryPrime [1,3,7,9,13,27]
    guard $ all (isNothing . tryPrime) [5,11,15,17,19,21,23,25]
  where
    nSq = n * n
    tryPrime c = isPrime (nSq + c)

result :: Integer
result = sum $ filter hasPrimePattern [1..1000000 * 150]
