module ProjectEuler.Problem25
  ( problem
  ) where

-- import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 25 Solved result

nfibs :: Int -> [Integer]
nfibs n = let r = replicate (n-1) 0 ++ 1 : 1 : zipWith ((-).(2*)) (drop n r) r in r

fib :: Int -> Integer
fib x = nfibs 2 !! x

result :: Int
-- TODO: magic number?
-- TODO: digitLen not working? (perhaps the number is too large)
result = head [x | x <-[4751..], (length . show . fib) x >= 1000 ]

