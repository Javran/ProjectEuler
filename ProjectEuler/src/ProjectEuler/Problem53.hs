module ProjectEuler.Problem53
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 53 Solved result

sel :: Integer -> Integer -> Integer
sel n r = product [n-r+1..n] `div` product [1..r]

-- NOTE: it's tempting to do the following
-- so that we can do
-- > 1 {* (n-r+1) / 1} {* (n-r+2) / 2} {* (n-r+3) / 3} ...
-- to minimize overflow Int ... but this doesn't help as the final result will
-- still blow up.
_sel :: Integer -> Integer -> Integer
_sel n r = foldl (\acc (x,y) -> acc * x `quot` y) 1 $ zip [n-r+1..n] [1..r]

result :: Int
result = length [ () | n <- [1..100], r <- [0..n], sel n r > 1000000 ]

