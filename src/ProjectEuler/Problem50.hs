module ProjectEuler.Problem50
  ( problem
  ) where

import Math.NumberTheory.Primes.Testing
import Control.Monad
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 50 Solved result

-- calculate sum of primes
--   keep the first 0 so that the sum of all primes
--   from p[1] to p[n], is just sum_p[n] - sum_p[0] = sum_p[n]
primeSum :: [Integer]
primeSum = 0 : zipWith (+) primeSum primes

-- only consider possible sums in the range
primeSumRanged :: [Integer]
primeSumRanged = takeWhile (<=1000000) primeSum

consecPrimeSum :: [Integer]
consecPrimeSum = do
    let lenMax = length primeSumRanged
    len <- [lenMax, lenMax-1..1]
    i <- [lenMax-1, lenMax-2..0]
    let j = i - len + 1
    guard $ 0 <= j && j < lenMax
    -- now we have i and j
    --   let's enumerate all possible consecutive sums
    --   random-access list support will be great
    --   but the search space isn't too scary.
    pure $ primeSumRanged !! i - primeSumRanged !! j

result :: Integer
result = head $ filter isPrime consecPrimeSum

