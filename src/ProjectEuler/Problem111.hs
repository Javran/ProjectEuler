module ProjectEuler.Problem111
  ( problem
  ) where

import Math.NumberTheory.Primes
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 111 Solved result

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

  Update: this idea turns out to be fairly efficient, yay!
 -}

{-
  `genPrimes d remainingDigit remainingCnt cur`
  generates a list of prime numbers based on:

  - d: the digit number that we want to repeat
  - remainingDigit: we still need to put this many repeated `d` in the number
  - remainingCnt: we still need to put this many digits to construct a number
    of desired length
  - cur: the number is built from most siginificant digit to the least,
    and this value stores constructed part of the number.

  e.g.: to generate primes of 4 digits with 3 repeated 1s:

  > genPrimes 1 3 4 0
  [1117,1151,1171,1181,1511,1811,2111,4111,8111]

 -}
genPrimes :: Int -> Int -> Int -> Integer -> [Integer]
genPrimes _ dRemain 0 cur = [ cur | dRemain == 0 && isPrime cur ]
genPrimes d remainingDigit remainingCnt cur = do
    -- to fill in this many digits,
    -- we should at least have this many places to begin with.
    guard $ remainingDigit <= remainingCnt
    repeatTheDigit <> fillInOtherDigits
  where
    repeatTheDigit = do
      guard $ remainingDigit > 0
      guard $ cur /= 0 || d /= 0
      let cur' = cur*10 + fromIntegral d
      genPrimes d (remainingDigit - 1) (remainingCnt - 1) cur'
    fillInOtherDigits = do
      x <- [0..9]
      guard $ x /= d
      guard $ cur /= 0 || x /= 0
      let cur' = cur * 10 + fromIntegral x
      genPrimes d remainingDigit (remainingCnt - 1) cur'

{-
  `findMaxRepeat n d` finds M(n,d),
  together with a prime satisfying the conditions as a representative.
 -}
findMaxRepeat :: Int -> Int -> (Int, Integer)
findMaxRepeat digitCount d = head $ foldMap search [9,8..]
  where
    search cnt = case genPrimes d cnt digitCount 0 of
      [] -> []
      x:_ -> pure (cnt,x)

{-
  For n = digitCount, generate the table from n to S(n,d),
  useful to perform a fact check again n=4 cases, whose answer is
  already given in the problem description.
 -}
findSum :: Int -> [(Int, Integer)]
findSum digitCount = (\x -> (x, findSumForD x)) <$> [0..9]
  where
    findSumForD :: Int -> Integer
    findSumForD d = sum (genPrimes d m digitCount 0)
      where
        (m,_) = findMaxRepeat digitCount d

{-
  First notice that 222...2 = 2 * 111...1,
  same for 333...3, 444...4 and so on.
  therefore there is only one number that repeats 10 times
  could be a prime, however:

  > factorise 1111111111
  [(11,1),(41,1),(271,1),(9091,1)]

  This is clearly a composite number. So we can safely
  start with attempting 9 repeated digits for all d = 0 .. 9
 -}
result :: Integer
result = sum $ snd <$> findSum 10
