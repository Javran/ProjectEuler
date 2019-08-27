module ProjectEuler.Problem123
  ( problem
  ) where

import Petbox

import ProjectEuler.Types
import ProjectEuler.Problem120 (f)

problem :: Problem
problem = pureProblem 123 Solved result

{-
  Note the similarity between this problem and Problem120,
  I think we can learn something from that one and come back with
  some more insights to work with.

  Update: turns out the exact same function used in Problem120
  is efficient enough to solve this problem as well.
 -}

oddPosPrimes :: [Integer]
oddPosPrimes = cuts primes
  where
    cuts (x:_:xs) = x : cuts xs
    cuts _ = error "unreachable"

posPrimePairs :: [(Int, Integer)]
posPrimePairs = zip [1,3..] oddPosPrimes

result :: Int
result = fst $ firstSuchThat (\(n,p) -> f p n > 10 ^! 10) posPrimePairs
