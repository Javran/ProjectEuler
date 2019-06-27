module ProjectEuler.Problem56
  ( problem
  ) where

import Data.List
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 56 Solved result

digitSum :: Integer -> Int
digitSum = sum . unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just (fromIntegral r, q)

result :: Int
result = maximum [ digitSum (a^b) | a <- [1..100], b <- [1..100 :: Int]]
