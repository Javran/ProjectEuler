module ProjectEuler.Problem148
  ( problem
  ) where

import Data.Monoid
import Data.List
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 148 Unsolved result

{-
  Idea: For Pascal's triangle,
  The number on n-th row and k-th column is:

  n choose k = [n / 1] * [(n-1) / 2] * [(n-2) / 3] * ... * [(n-k+1) / k]

  and here we want to find the number of elements not divisible by 7
  in the first x rows of Pascal's triangle.

  Here Lucas's theorem sounds promising: https://en.wikipedia.org/wiki/Lucas's_theorem,
  namely: A binomial coefficient binomial n k is divisible by a prime p
  if and only if at least one of the base p digits of k is greater than the corresponding digit of n.

  After comparing the result implemented with Lucas's theorem with naive one,
  we have confirmed its correctness, and allow us to have some speed up.

  This isn't fast enough though, but the next potential speed up is obvious:
  given a number n, we want to know how many numbers in [0..n] has at least one of base 7 digit
  greater than corresponding digit of n.

 -}

{-
  Tell if binomial n k is divisible by 7.

  The implementation uses Lucas's theorem.
 -}
binomialDivBy7 :: Int -> Int -> Bool
binomialDivBy7 n k = any (uncurry (<)) $ zip nIn7Rev kIn7Rev
  where
    nIn7Rev = toBase7Rev n
    kIn7Rev = toBase7Rev k

toBase7Rev :: Int -> [Int]
toBase7Rev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 7 in Just (r, q)

checkRow n = foldMap (\k -> if binomialDivBy7 n k then 0 :: Sum Int else 1) [0..n]

result = getSum $ foldMap checkRow [0 :: Int .. 10 ^! 3 - 1]
{-
  Some results obtained from current implementation:
  - [0 .. 10^3 - 1] => 118335
  - [0 .. 10^4 - 1] => 6264360
 -}
