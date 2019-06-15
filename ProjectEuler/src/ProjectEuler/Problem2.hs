module ProjectEuler.Problem2 (problem) where

import Data.List
import Data.List.Split
import Data.Monoid

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 2 Solved result

fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- result :: Int
result =
  sum . filter even      -- sum of evens
  . takeWhile (<=4000000)  -- do not exceed four million
  $ drop 2 fibs 
