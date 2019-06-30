module ProjectEuler.Problem9
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 9 Solved result

result :: Int
result = x * y * (1000 - x - y)
  where
    sq n = n * n
    (x,y):_ = [ (a,b)
              | a <- [0..1000 :: Int]
              , b <- [a+1..1000]
              , a < b
              , b < (1000-a-b)
              , sq a + sq b == sq (1000-a-b)
              ]

