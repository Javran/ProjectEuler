module ProjectEuler.Problem123
  ( problem
  ) where

import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 123 Unsolved result

{-
  Note the similarity between this problem and Problem120,
  I think we can learn something from that one and come back with
  some more insights to work with.
 -}

f :: Integer -> Int -> Integer
f a n =
  if even n
    then 2
    else
      let n' = fromIntegral n
      in (2 * n' * a) `rem` (a * a)

oddPosPrimes :: [Integer]
oddPosPrimes = cuts primes
  where
    cuts (x:_:xs) = x : cuts xs
    cuts _ = error "unreachable"

posPrimePairs :: [(Int, Integer)]
posPrimePairs = zip [1,3..] oddPosPrimes

result :: Int
result = fst $ firstSuchThat (\(n,p) -> f p n > 10 ^! 10) posPrimePairs
