module ProjectEuler.Problem2 (problem) where

import Data.List

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 2 Solved result

fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

{-
  Note that the parity pattern of Fibonacci sequence is obvious:
  if we begin with 0, then the squence's parity will be: cycle [even, odd, odd],
  this allows us to create the Fibnoacci sequence with only even numbers
  by simply "take one and skip next two" over and over again.
 -}
fibEvens :: [Int]
fibEvens = unfoldr (Just . onlyEvens) fibs
  where
    onlyEvens (a:_:_:b) = (a,b)
    onlyEvens _ = error "unreachable"

result :: Int
result =
  sum
  . takeWhile (<= 4000000)
  $ fibEvens
