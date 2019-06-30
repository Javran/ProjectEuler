module ProjectEuler.Problem41
  ( problem
  ) where

import Math.NumberTheory.Primes.Testing
import Petbox hiding (isPrime)

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 41 Solved result

result :: Int
result =
  maximum
    [ x
    | l <- [1..9]
    , x <- digitsToInt <$> permutations [1..l]
    , isPrime . fromIntegral $ x
    ]
