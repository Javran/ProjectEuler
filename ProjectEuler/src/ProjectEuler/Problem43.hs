module ProjectEuler.Problem43
  ( problem
  ) where

import ProjectEuler.Problem24 (getPermutation, factorial)
import ProjectEuler.Types

{-
  TODO:
  potential optimization:

  the problem with current approach is that we need to check
  every possible permutation to find the exhaustive list.

  but why don't we begin with a smaller branching factor -
  pick d10, d9, d8 first with 3 distinct numbers, verify they
  are divisible by 17, then choose d7, check with 13, then choose d6,
  check with 11, etc.
 -}

problem :: Problem
problem = pureProblem 43 Solved result

toNum :: [Int] -> Int
toNum = foldl (\acc i -> acc*10 + i) 0

valid :: [Int] -> Bool
valid xs = and $ zipWith (\num prime -> num `rem` prime == 0) numList primeList
  where
    numList = [indToNum (x,x+1,x+2) | x <- [1..7]]
    indToNum (a,b,c) = 100 * (xs !! a) + 10 * (xs !! b) + (xs !! c)
    primeList = [2,3,5,7,11,13,17]

result :: Int
result =
  sum $ map toNum $ filter valid $ (`getPermutation` [0..9]) <$> [0..factorial 10 -1]
