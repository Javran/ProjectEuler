module ProjectEuler.Problem4
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 4 Solved result

isPalindromic :: (Show a, Integral a) => a -> Bool
isPalindromic x = xs == reverse xs
  where
    xs = show x

result :: Int
result =
  maximum
  $ filter isPalindromic
    [ x*y | x <- [999,998..100] , y <- [x, x-1 .. 100]]

