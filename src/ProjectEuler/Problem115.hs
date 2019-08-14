module ProjectEuler.Problem115
  ( problem
  ) where

import Data.MemoTrie (memoFix)
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 115 Solved result

{-
  Note that this problem is a generalization of Problem114,
  if we can parameterize on m from Problem114,
  we should be at least half way on the actual solution.
 -}

{- TODO: perhaps reuse code in Problem114 -}
fillCount :: Int -> Int -> Integer
fillCount m n = 1 + sum (f <$> [1..n])
  where
    f :: Int -> Integer
    f = memoFix pf
      where
        pf f' i =
          case compare i m of
            LT -> 0
            EQ -> 1
            _ ->
              let g l = sum (f' <$> [m..i-l-1])
              in fromIntegral (i-m+1) + sum (g <$> [m..i])

result :: Int
result = firstSuchThat (\n -> fillCount 50 n > 1000000) [51..]
