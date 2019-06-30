module ProjectEuler.Problem30
  ( problem
  ) where

import Data.Char

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 30 Solved result

testAllow :: Int -> Bool
testAllow x = sum (map ((^(5 :: Int)) . digitToInt) (show x)) == x

result :: Int
result = sum (filter testAllow [10.. (9^(5 :: Int) * 5)])

