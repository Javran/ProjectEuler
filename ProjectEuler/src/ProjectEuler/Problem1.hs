module ProjectEuler.Problem1 (problem) where

import ProjectEuler.Types

problem :: Problem
problem = Problem 1 Solved $ const main

main :: IO ()
main = print
     $ sum [ x
           | x <- [1::Int .. 999]
           , x `mod` 3 == 0 || x `mod` 5 == 0
           ]
