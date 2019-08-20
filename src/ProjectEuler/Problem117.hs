module ProjectEuler.Problem117
  ( problem
  ) where

import ProjectEuler.Types
import Data.Monoid
import Data.MemoTrie (memoFix)

problem :: Problem
problem = pureProblem 117 Solved result

{-
  Despite that this problem is based on Problem116,
  I doubt how much code we can reuse, as now tiles of
  diffferent lengths are allowed to mix.

  `ballsInBins` could still be useful,
  but if we were to use it, we need to find all ways
  of constructing up to n tiles using tiles of length 2,3,4,
  and then fill in spaces...

  Let's forget about it and try the same method as Problem114.

  Let f(i) be the ways to occupy from 1 to i (inclusive),
  and all these ways has to:

  - at least have one block in it.
  - the last block must end at position i.

  f(0) = 0
  f(1) = 0
  f(2) = 1
       [XX]
  f(3) = 2
       [_XX]
    or [XXX]
  f(4) = 4
       [__XX]
    or [_XXX]
    or [XXXX]
    or [XX|XX]

  starting from f(5), there's always 3 "nothing but last block" cases,
  they cannot be passed down from any previous f(?).
  (In fact, the purpose of listing values from f(0) to f(4) is to avoid
  conditioning on cases where some shorter block can fit but longer ones can't)

  Now let length of last block be l, and the total length is i
  we need to find all ways to occupy from block 1 to block (i-l),

  f(i) = 3 + sum of f(j), where l = 2..4, j = 1..i-l

 -}

f :: Int -> Integer
f = memoFix f'
  where
    f' pf i = case i of
      0 -> 0
      1 -> 0
      2 -> 1
      3 -> 2
      4 -> 4
      _ -> 3 + getSum (foldMap Sum [pf j | l <- [2..4], j <- [1..i-l]])

result :: Integer
result = 1 + getSum (foldMap (Sum . f) [1..50])

