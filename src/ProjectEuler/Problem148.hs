module ProjectEuler.Problem148
  ( problem
  ) where

import Data.Monoid
import Data.Word
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 148 Unsolved result

{-
  Note: old implementation removed. see history of this file for detail.

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

  Some after thoughts:

  - Continuing from f(n) defined above, I realized that to calculate f(7n+b), one need to compute f(n) first,
    so we can batch some work together by computing f(7n+0)+f(7n+1)+...+f(7n+b) in one go, which is exactly
    what fSum defined below does.

  - Some claimed the result looks like a Sierpinski triangle - I haven't realize that though.

 -}

{- Implements f(n) as described above -}
f :: Int -> Word64
f m
  | m < 7 = 0
  | otherwise = let (n,b) = m `quotRem` 7 in fInt (b+1)*f n + fInt (n * (6-b))

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
  => (f n * (b*b + 3*b+2) + n * (12 + 11*b - b*b)) / 2

 -}
fSum :: Int -> Sum Word64
fSum m
  | m < 7 = 0
  | otherwise =
    let (n,b) = m `quotRem` 7
        sumCur = (fInt (b*b + 3*b + 2)*f n + fInt (n*(12 + 11*b - b*b))) `quot` 2
    in Sum sumCur <> fSum (7*n-1)

{-
  Because 2^59 < (n+1) * (n+2) < 2^60, to be compliant with the standard in which
  Int is only guaranteed to be in range -2^29 .. 2^29-1, we'll use Word64 here.
 -}
result :: Word64
result = fInt ((n+1)*(n+2) `quot` 2) - getSum (fSum n)
  where
    n = 10 ^! 9 - 1
