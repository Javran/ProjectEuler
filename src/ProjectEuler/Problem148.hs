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

  Let f(n) be the number of numbers within [0..n], who has
  at least one digit of base 7 digit greater than corresponding digit of n.
  (I'll say a number "qualifies" to mean this below)

  It should be possible to compute f(7n + b) where b <- [0..6] from f(n).

  There are n+1 numbers in [0..n], and:
  - f(n) of those qualifies.
  - n+1-f(n) of those does not qualify.

  For f(7n+b):
  - 7*f(n) qualifies immediately by taking those that qualifies as f(n)
    and padding a 0 in the end (as the least siginificant digit)
  - Now we just need to look at the last digit: there are 6-b elements in [b+1..6]
    those that does not qualify for f(n) can be made qualified for f(7n+b)
    by taking one of those numbers, this accounts for (n+1-f(n)-1)*(6-b) numbers.
    Note that we are removing one more, we can be demonstrated by an example:

    for 7n+b = 12342 (all literals for this example is in base 7),
    1234 does not qualify for 1234, and we need to therefore exclude [12343..12346],

    For now I think those are the only cases where a special handling is needed.

  So, in short:
  f(b) = 0 (b < 7)
  f(7n+b) = 7*f(n) + (n+1-f(n)-1)*(6-b) = (b+1)*f(n) + n*(6-b)

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
    toBase7Rev = unfoldr go
      where
        go 0 = Nothing
        go v = let (q,r) = v `quotRem` 7 in Just (r, q)

_countRow :: Int -> Sum Int
_countRow n = foldMap (\k -> if binomialDivBy7 n k then 0 else 1) [0..n]

{- Implements f(n) as described above -}
f :: Int -> Int
f m
  | m < 7 = 0
  | otherwise = let (n,b) = m `quotRem` 7 in (b+1)*f n + n*(6-b)

{-
  Note that the process of computing f(7 n + b) all require computing f(n),
  we might as well batch those computations together:

  fSum(m) = sum of f(i), where i <- [0..m].

  If m = 7*n + b (0 <= b < 7):

  fSum(m) = fSum(7*n-1) + [ f(7*n+i) | i <- [0..b] ]

  [ f(7*n+i) | i <- [0..b] ]
  => [ (i+1)*f n + n*(6-i) | i <- [0..b] ] (definition)
  => f n * [ i+1 | i <- [0..b] ] + n * [ 6-i | i <- [0..b] ]
  => (f n * (b+1) * (b+2) + n * (42 - (5-b)*(6-b))) / 2
  => (f n * (b+1) * (b+2) + n * (12 + 11*b - b*b)) / 2

 -}
fSum :: Int -> Int
fSum m
  | m < 7 = 0
  | otherwise =
    let (n,b) = m `quotRem` 7
        sumCur = ((b+1)*(b+2)*f n + n*(12 + 11*b - b*b)) `quot` 2
    in sumCur + fSum (7*n-1)

_countRowFast :: Int -> Sum Int
_countRowFast n = Sum $ n + 1 - f n

result :: Int
result = ((n+1)*(n+2) `quot` 2)  - fSum n
  where
    n = 10 ^! 9 - 1
{-
  Some results obtained from current implementation:
  - [0 .. 10^3 - 1] => 118335
  - [0 .. 10^4 - 1] => 6264360
  - [0 .. 10^5 - 1] => 346238256
  - [0 .. 10^7 - 1] => 788306648416
 -}
