module ProjectEuler.Problem36
  ( problem
  ) where

import Data.List
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 36 Solved result

result :: Int
result = sum
  [ x
  | x<-[1..1000000-1]
    -- the ordering is intentional: it's more likely and efficient
    -- to check for decimals before checking binaries.
  , x == numReverseInBase 10 x
  , x == numReverseInBase 2 x
  ]

-- convert to reversed list of digits and put digits together,
-- this allows digit-wise reversal of numbers
-- and gives a compact representation (i.e. the number itself) to work with
-- rather than comparing on list of stuff.
numReverseInBase :: Int -> Int -> Int
numReverseInBase base = foldl (\a b -> a*base+b) 0 . unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` base in Just (r, q)
