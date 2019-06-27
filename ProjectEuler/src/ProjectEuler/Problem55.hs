module ProjectEuler.Problem55
  ( problem
  ) where

import Data.List
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 55 Solved result

isPalindrome :: Integer -> Bool
isPalindrome x = x == numReverse x

-- TODO: this is the same function in Problem36.
numReverse :: Integer -> Integer
numReverse = foldl (\a b -> a*10+b) 0 . unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just (r, q)

isLychrel :: Integer -> Bool
isLychrel x = not . any isPalindrome . take 50 . tail $ iterate next x
  where
    next y = y + numReverse y

result :: Int
result = length . filter isLychrel $ [1..10000-1]
