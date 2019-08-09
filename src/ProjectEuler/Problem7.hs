module ProjectEuler.Problem7
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 7 Solved result

result :: Int
result =  primes !! (10001-1) -- since list indices are 0-based
