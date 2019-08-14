module ProjectEuler.Problem115
  ( problem
  ) where

import Data.MemoTrie (memoFix)
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 115 Unsolved result

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

result :: Bool
result =
  and [ fillCount 3 29 == 673135
      , fillCount 3 30 == 1089155
      , fillCount 10 56 == 880711
      , fillCount 10 57 == 1148904
      ]
