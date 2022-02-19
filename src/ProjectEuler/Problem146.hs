{-# LANGUAGE BlockArguments #-}

module ProjectEuler.Problem146
  ( problem
  )
where

import Control.Monad
import qualified Data.IntSet as IS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import Math.NumberTheory.Primes
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
  **n == none of [2,6,7,11] (mod 13)**
 -}

hasPrimePattern :: Int -> Bool
hasPrimePattern n =
  all tryPrime [1, 3, 7, 9, 13, 27]
    && not (any tryPrime [5, 11, 15, 17, 19, 21, 23, 25])
  where
    tryPrime c = isJust $ isPrime $ n * n + c

{-
  This function encodes the "mod p" reasoning described above
  in a guarding function.
 -}
mkFilter :: Int -> Int -> Bool
mkFilter p =
  if IS.size allowed < IS.size denied
    then isAllowed
    else isNotDenied
  where
    isAllowed v = (v `rem` p) `IS.member` allowed
    isNotDenied v = (v `rem` p) `IS.notMember` denied
    cs = IS.fromDistinctAscList [1, 3, 7, 9, 13, 27 :: Int]
    -- n^2 should not equal to any of those under mod operation.
    xs = IS.map (\n -> p - (n `rem` p)) cs
    -- a small hack here: the "correct" method is to start from 0,
    -- for most of the primes that we are testing however,
    -- starting from 0 only slows it down.
    lo = if p == 29 then 0 else 1
    (allowed, denied) =
      IS.partition
        (\n -> (n * n `rem` p) `IS.notMember` xs)
        $ IS.fromDistinctAscList [lo .. p -1]

fastSieve :: Int -> Bool
fastSieve n = all (\v -> mightBePrime (n * n + v)) [1, 3, 7, 9, 13, 27]
  where
    -- this relies on the assumption that all those primes are less than v,
    -- which is true given that n is started at 10,
    -- therefore v is at least 101
    mightBePrime v =
      all
        (\p -> v `rem` p /= 0)
        {-
          this list of prime is randomly tuned.
          the idea here is to do some cheap filtering
          before calling the expensive "isPrime" operation.
         -}

        [2, 3, 11, 13, 29, 37]

-- some tunable small primes for quicking generating a firstCycle.
smallPrimes :: [Int]
smallPrimes = 2 : 5 : [3, 7, 11, 13, 17, 29]

-- drop 2,5 as the check is unnecessary given the way we generate the list.
pFilters :: [Int -> Bool]
pFilters = mkFilter <$> drop 2 smallPrimes

result :: Int
result =
  sum
    . filter (\v -> fastSieve v && hasPrimePattern v)
    . takeWhile (<= 1000000 * 150)
    $ getCandidate <$> [0 ..]
  where
    vLen = VU.length firstCycleV
    getCandidate i = q * cycleLen + firstCycleV VU.! r
      where
        (q, r) = i `quotRem` vLen

cycleLen :: Int
cycleLen = product smallPrimes

firstCycleV :: VU.Vector Int
firstCycleV = VU.fromList do
  -- only do detailed checking on first cycle,
  -- after that we can simply reuse this checked result
  n <- [10, 20 .. cycleLen]
  guard $ (n `rem` 3) `elem` [1, 2]
  guard $ (n `rem` 7) `elem` [3, 4]
  guard $ (n `rem` 11) `elem` [1, 4, 5, 6, 7, 10]
  guard $ (n `rem` 13) `notElem` [2, 6, 7, 11]
  guard $ all ($ n) pFilters
  pure n
