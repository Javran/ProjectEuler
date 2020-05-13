module ProjectEuler.Problem72
  ( problem
  ) where

import Data.Int
import Math.NumberTheory.Primes hiding (isPrime)
import Math.NumberTheory.Primes.Testing

import qualified Data.List.Ordered as LO

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 72 Solved result

-- https://oeis.org/A015614
phi :: Int -> Int
phi 1 = 1
phi n =
  if isPrime (fromIntegral n)
    then n-1
    else (n `div` denom) * numer
 where
   nFactorised = factorise n
   uprimes = LO.sort . map (fromIntegral . unPrime . fst) $ nFactorised
   numer = product . map (subtract 1) $ uprimes
   denom = product uprimes

-- because the approximated result exceeds the guaranteed range
-- an Int can hold, we are using Int64 to make it safe

faySum :: Int -> Int64
faySum n = sum (fromIntegral . phi <$> [1..n]) - 1

-- approximated result
-- see: http://en.wikipedia.org/wiki/Farey_sequence
-- print (3 * n' * n' / (pi^!2) :: Double)

result :: Int64
result = faySum 1000000
