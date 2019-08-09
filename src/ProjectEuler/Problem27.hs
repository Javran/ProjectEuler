module ProjectEuler.Problem27
  ( problem
  ) where

import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List
import Data.Ord
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 27 Solved result

consePrimeLen :: Int -> Int -> Int
consePrimeLen a b =
  length $ takeWhile (isPrime . fromIntegral) [ (n+a)*n+b | n <- [0..] ]

result :: Int
result = u*v
  where
    ((u,v), _) = maximumBy (comparing snd) searchSpace
    searchSpace =
      [ ((c,d), consePrimeLen c d)
      | a <- [0..1000]
      , b <- takeWhile (<1000) primes
      , gcd a b == 1
      , c <-[-a,a]
      , d <-[-b,b]
      , c + d > 0
      ]
