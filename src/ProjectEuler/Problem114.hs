module ProjectEuler.Problem114
  ( problem
  ) where

import Data.MemoTrie (memoFix)

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 114 Solved result

{-
  Idea: Let's do dynamic programming on this one:

  Let f(i) be the ways to occupy from 1 to i (inclusive),
  and all these ways has to:

  - at least have one block in it.
  - the last block must end at position i.

  therefore:

  f(i) = 0 (i < 3)
  f(3) = 1
       [XXX]
  f(4) = 2
       [XXXX]
    or [_XXX]
  f(5) = 3
       [XXXXX]
    or [_XXXX]
    or [__XXX]
  f(6) = 4
       [XXXXXX]
    or [_XXXXX]
    or [__XXXX]
    or [___XXX]
  f(7) = 6
       [XXXXXXX]
    or [_XXXXXX]
    or [__XXXXX]
    or [___XXXX]
    or [____XXX]
    or [XXX_XXX]

  then the solution should be 1 + sum of f(i) for all i,
  and the missing case (the "1 +" bit) corresponds to "no block at all".

  To avoid counting duplicated cases, let's consider
  the final block being of length l:
  (from problem description we know l >= 3)

  - note that for all l = 3 to i, we can do a "nothing but last block" case,
    which accounts for i-3+1 = i-2 cases.
  - besides the case above, for 3 <= j <= i-l-1, we can sum up f(j) for 3 <= l <= i.

original implementation for this is:

f i
  | i < 3 = 0
  | i == 3 = 1
  | otherwise =
      let g :: Int -> Int
          g l = sum (f <$> [3..i-l-1])
      in (i-2) + sum (g <$> [3..i])
 -}

f :: Int -> Integer
f = memoFix pf
  where
    pf f' i =
      case compare i 3 of
        LT -> 0
        EQ -> 1
        _ ->
          let g l = sum (f' <$> [3..i-l-1])
          in fromIntegral (i-2) + sum (g <$> [3..i])

result :: Integer
result = 1 + sum (f <$> [1..50])
