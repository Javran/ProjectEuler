module ProjectEuler.Problem4
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 4 Solved result

result = ()

isPalindromic :: (Show a, Integral a) => a -> Bool
isPalindromic x = xs == reverse xs
  where
    xs = show x

main :: IO ()
main = print
     . maximum
     $ filter isPalindromic
              [ x*y
              | x <-[999,998..100 :: Int]
              , y <-[x,(x-1)..100]
              ]

