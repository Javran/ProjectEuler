module ProjectEuler.Problem24
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 24 Solved result

factorials :: [Int]
factorials = scanl (*) 1 [1..]

factorial :: Int -> Int
factorial = (factorials !!)

{-
  Given a number p in [0 .. (n!-1)], return the corresponding permutation
  by using elements in the given list.

  for n = 3, xs = [0,1,2], we have:

  - getPermutation 0 xs = [0,1,2]
  - getPermutation 1 xs = [0,2,1]
  - getPermutation 2 xs = [1,0,2]
  - getPermutation 3 xs = [1,2,0]
  - getPermutation 4 xs = [2,0,1]
  - getPermutation 5 xs = [2,1,0]

  note that the permutation can be broken down into 3 groups by first element:

  - group 0: 0 then <permutations of {1,2}>,
    or the result of `getPermutation p' [1,2]` with p' being relative to this group
  - group 1: 1 then <permutations of {0,2}>
    or the result of `getPermutation p' [0,2]` with p' being relative to this group
  - group 2: 2 then <permutations of {0,1}>
    or the result of `getPermutation p' [0,1]` with p' being relative to this group
 -}
getPermutation :: Int -> [Int] -> [Int]
getPermutation _ xs@[_] = xs
getPermutation p xs = h : getPermutation p' t
  where
    n = length xs
    subCycle = factorial $ n - 1
    (headInd, p') = p `quotRem` subCycle
    (h,t) = breakList headInd xs

-- remove the n-th element from the list,
-- return the removed element and list after removal.
breakList :: Int -> [a] -> (a, [a])
breakList n xs = (z, ys<>zs)
  where
    (ys,z:zs) = splitAt n xs

result :: Integer
result =
  foldl (\a i -> a*10 + fromIntegral i) 0
  $ getPermutation 999999 [0..9]
