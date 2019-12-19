{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem148
  ( problem
  ) where

import Math.Combinat.Numbers.Sequences
import ProjectEuler.Types

problem :: Problem
problem = Problem 148 Unsolved run

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

checkRow n = (n,) <$> filter ((== 0) . (`rem` 7) . binomial n) [0..n]

run = mapM_ logT $ filter (not . null) $ fmap checkRow [1 :: Int ..100]
