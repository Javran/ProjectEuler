module ProjectEuler.Problem111
  ( problem
  ) where

import Math.NumberTheory.Primes
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 111 Unsolved result

{-
  There's about ~400000000 prime numbers that has 10 digits,
  checking all of them will take too long.

  First step: we can begin with figuring out M(10,d) for d = 0..9,
  the idea is to generate numbers with 10 repeated number, 9 repeated number, etc.
  until we find one prime, at which point we'll known M(10,d) for
  that particular d. After this step is done,
  we can proceed to generate numbers with M(10,d) repeated numbers,
  test again, to eventually get all primes of that pattern and
  therefore obtain S(10,d).
 -}

genPrimes _ d 0 cur = [ cur | d == 0 && isPrime cur ]
genPrimes d remainingDigit remainingCnt cur =
  (do
    guard $ remainingDigit > 0
    let cur' = cur*10 + fromIntegral d
    genPrimes d (remainingDigit - 1) (remainingCnt - 1) cur')
  <> (do x <- [0..9]
         guard $ x /= d
         guard $ cur /= 0 || x /= 0
         let cur' = cur * 10 + fromIntegral x
         genPrimes d remainingDigit (remainingCnt - 1) cur'
     )

result = genPrimes 1 3 4 0
