module ProjectEuler.Problem21
  ( problem
  ) where

import Math.NumberTheory.ArithmeticFunctions
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 21 Solved result

divSum :: Int -> Int
divSum x = sum (divisors x) - x

isAmicable :: Int -> Int -> Bool
isAmicable x y = x == divSum y && y == divSum x

result :: Int
result = sum [ x+y | x <- [1..10000], let y = divSum x, x < y, isAmicable x y ]

