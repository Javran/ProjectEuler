module ProjectEuler.Problem65
  ( problem
  ) where

import Data.Ratio
import Data.List

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 65 Solved result

intToDigitsRev :: Integer -> [Int]
intToDigitsRev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just (fromIntegral r, q)

eExpand :: [Integer]
eExpand = 2 : concatMap (\x -> [1,2*x,1]) [1..]

approx :: [Integer] -> Rational
approx [] = error "invalid input"
approx [x] = x%1
approx (x:xs) = (x%1) + (1%1) / approx xs

result :: Int
result =
  sum
  . intToDigitsRev
  . numerator
  . approx
  $ take 100 eExpand
