module ProjectEuler.Problem146
  ( problem
  ) where

import Control.Monad
import Data.Maybe
import Math.NumberTheory.Primes

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 146 Solved result

{-
  Idea: let's do it the stupid way, maybe there are some insights.

  Note: the brute force actually works.
  It'll take few minutes to filter through n such that
  n^2 + c where c <- [1,3,7,9,13,27] are all primes,
  then we print that list, and apply result of the filtering
  to make sure all of those are consecutive.

  Now, to show that n == 0 (mod 10):

  first observe:
  n^2 + c /= 0 (mod 2)
  where c <- [1,3,7,9,13,27] == [1] (mod 2)
  therefore:
  n^2 + c = n^2 + 1 /= 0 (mod 2)
  n^2 /= 1 (mod 2)
  => **n == 0 (mod 2)**

  Also c <- [1,3,7,9,13,27] == [1,2,3,4] (mod 5)
  therefore
  - n^2 + 1 /= 0 (mod 5)
  - n^2 + 2 /= 0 (mod 5)
  - ...

  - n^2 /= 4,3,2,1 (mod 5)
  - by attempting n = 1..4, we have: **n == 0 (mod 5)**

  so n == 0 (mod 2) && n == 0 (mod 5) => n == 0 (mod 10)

  We can also do something similar with (mod 3) and (mod 7):

  c <-[1,3,7,9,13,27] == [0,1] (mod 3)

  - n^2 + 0 /= 0 (mod 3)
  - n^2 + 1 /= 0 (mod 3)

  - n^2 /= 0 (mod 3)
  - n^2 /= 2 (mod 3)

  **n == 1 or 2 (mod 3)**

  c <- [1,3,7,9,13,27] == [0,1,2,3,6] (mod 7)
  - n^2 /= 6 (mod 7)
  - n^2 /= 5 (mod 7)
  - n^2 /= 4 (mod 7)
  - n^2 /= 1 (mod 7)

  **n == 3 or 4 (mod 7)**

  similar analysis can be done for mod 11:

  **n == one of [1,4,5,6,7,10] (mod 11)**

  for mod 13:
  **n == none of [2,6,7,11] (mod 11)**
 -}

hasPrimePattern :: Int -> Bool
hasPrimePattern n =
    all tryPrime [1,3,7,9,13,27]
    && all (not . tryPrime) [5,11,15,17,19,21,23,25]
  where
    nSq = n * n
    tryPrime c = isJust $ isPrime (nSq + c)

{-
  This function encodes the "mod p" reasoning described above
  in a guarding function.
 -}
mkFilter :: Int -> Int -> Bool
mkFilter p = isAllowed
  where
    isAllowed v = (v `rem` p) `elem` allowed
    cs = [1,3,7,9,13,27 :: Int]
    -- n^2 should not equal to under mod operation.
    xs = LOrdered.nubSort (fmap ((p -) . (`rem` p)) cs)
    allowed =
      filter (\n -> (n*n `rem` p) `notElem` xs) [1..p-1]

result :: Int
result =
    sum
    . filter hasPrimePattern
    . takeWhile (<= 1000000 * 150)
    $ concat (iterate (fmap (+ cycleLen)) firstCycle)
  where
    smallPrimes = [2,3,5,7,11,13]
    [f3,f7,f11,f13] = fmap mkFilter [3,7,11,13]
    cycleLen = product smallPrimes
    -- only do detailed checking on first cycle,
    -- after that we can simply reuse this checked result
    firstCycle = do
      n <- [10,20..cycleLen]
      guard $ f3 n
      guard $ f7 n
      guard $ f11 n
      guard $ f13 n
      pure n
