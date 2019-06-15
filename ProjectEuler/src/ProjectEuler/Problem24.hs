module ProjectEuler.Problem24
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 24 Solved result

factorials :: [Int]
factorials = scanl (*) 1 [1..]

factorial :: Int -> Int
factorial = (factorials !!)

-- define `permutation id` x to be the identity of the (x+1)-th permutaion
-- e.g. 0,1,2, pid 0 => 0,1,2, pid 1 => 0,2,1, ... etc.
-- the problem is to find the (999999+1)-th permutation for [0..9], pid = 999999
pidToPermutation :: Int -> [Int] -> [Int]
pidToPermutation 0 arr = arr
pidToPermutation pid arr = h : pidToPermutation subPid t
  where
    n = length arr
    subCycle = factorial $ n - 1
    (headInd, subPid) = pid `quotRem` subCycle
    (h,t) = breakList headInd arr

-- TODO: break list

-- get the x-indexed element, return the list after removing that element
breakList :: (Eq a) => Int -> [a] -> (a,[a])
breakList 0 (x:xs) = (x,xs)
breakList n (x:xs) = (x1,x:xs1)
  where
    (x1,xs1) = breakList (n-1) xs

result :: String
result = (\x -> ['0'..] !! x) <$> pidToPermutation 999999 [0..9]
