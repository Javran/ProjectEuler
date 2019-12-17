module ProjectEuler.Problem146
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Primes.Testing

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

hasPrimePattern :: Integer -> Bool
hasPrimePattern n =
    all tryPrime [1,3,7,9,13,27]
    && all (not . tryPrime) [5,11,15,17,19,21,23,25]
  where
    nSq = n * n
    tryPrime c = isPrime (nSq + c)

result :: Integer
result =
  sum
  . filter hasPrimePattern
  $ do
    n <- [10,20..1000000 * 150]
    guard $ (n `rem` 3) `elem` [1,2]
    guard $ (n `rem` 7) `elem` [3,4]
    guard $ (n `rem` 11) `elem` [1,4,5,6,7,10]
    guard $ (n `rem` 13) `notElem` [2,6,7,11]
    pure n
