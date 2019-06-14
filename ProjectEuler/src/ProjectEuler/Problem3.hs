module ProjectEuler.Problem3 (problem) where

import Data.Numbers.Primes
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 3 Solved result

n :: Integer
n = 600851475143

result :: Integer
result =
  head . filter (\x -> n `mod` x == 0)
  . reverse
  . takeWhile (\x -> x*x <= n)
  $ primes

