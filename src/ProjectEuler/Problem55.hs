module ProjectEuler.Problem55
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 55 Solved result

isPalindrome :: Integer -> Bool
isPalindrome x = x == numReverse x

numReverse :: Integer -> Integer
numReverse = numReverseInBase 10

isLychrel :: Integer -> Bool
isLychrel x = not . any isPalindrome . take 50 . tail $ iterate next x
  where
    next y = y + numReverse y

result :: Int
result = length . filter isLychrel $ [1..10000-1]
