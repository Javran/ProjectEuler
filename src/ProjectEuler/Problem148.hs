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

  Okay, some interesting findings:

  - https://en.wikipedia.org/wiki/Lucas's_theorem
  - Some divisibility properties of binomial coefficients, Daniel Yaqubi, Madjid Mirzavaziri

  > An elementary property of binomial coefficients is that "n choose k"
  is divisible by a prime p for all 1 < k < n if and only if n is a power of p.

  > A binomial coefficient binom {m}{n} is divisible by a prime p
  if and only if at least one of the base p digits of n is greater than the corresponding digit of m.

 -}

{-
  Tell if binomial n k is divisible by 7.

  The following implementation is based on Lucas's theorem (https://en.wikipedia.org/wiki/Lucas's_theorem),
  which says, A binomial coefficient binomial n k is divisible by a prime p
  if and only if at least one of the base p digits of k is greater than the corresponding digit of n.

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

result = getSum $ foldMap checkRow [0 :: Int .. 10 ^! 4 - 1]
