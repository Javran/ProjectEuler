{-# LANGUAGE BangPatterns #-}
module ProjectEuler.Problem112
  ( problem
  ) where

import Data.List
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 112 Solved result

{-
  turns out brute forcing this is good enough..
 -}
isNondecreasing :: [Int] -> Bool
isNondecreasing xs = and (zipWith (<=) xs (tail xs))

isBouncy :: Int -> Bool
isBouncy z = not (isNondecreasing xs) && not (isNondecreasing ys)
  where
    xs = intToDigits z
    ys = intToDigitsRev z

partitionCount :: (a -> Bool) -> [a] -> [(Int, Int)]
partitionCount f =
  scanl' (\(!tCnt,!fCnt) x -> if f x then (tCnt+1,fCnt) else (tCnt,fCnt+1)) (0,0)

result :: Int
result = u + v
  where
   {-
     We want: x / x+y == 99 / 100 => x+y /= 0 and x == 99 * y.
     so we just want to take the second value from partitionCount
     (first one is the initial value, namely (0,0))
    -}
   _:(u,v):_ =
     filter (\(x,y) -> x == 99 * y)
     $ partitionCount isBouncy [1..]

