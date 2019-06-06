module ProjectEuler.Problem1 (P) where

import ProjectEuler.Types

data P

instance Problem P where
  getStatus _ = Solved
  run _ _ = print
     $ sum [ x
           | x <- [1::Int .. 999]
           , x `mod` 3 == 0 || x `mod` 5 == 0
           ]
