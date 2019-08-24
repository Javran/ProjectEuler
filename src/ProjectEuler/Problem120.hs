module ProjectEuler.Problem120
  ( problem
  ) where

import Control.Monad
import Petbox
import ProjectEuler.Types
import Math.NumberTheory.Powers.Modular

problem :: Problem
problem = pureProblem 120 Unsolved result

{-
  Idea: not sure what to do yet, see if I can find some pattern:

  > f 6 <$> [0..20] -- 6
  [2,12,2,0,2,24,2,12,2,0,2,24,2,12,2,0,2,24,2,12,2]

  > f 7 <$> [0..20]
  [2,14,2,42,2,21,2,0,2,28,2,7,2,35,2,14,2,42,2,21,2] -- 14

  > f 8 <$> [0..20] -- 2
  [2,16,2,48,2,16,2,48,2,16,2,48,2,16,2,48,2,16,2,48,2]

  > f 11 <$> [0 .. 40] -- 22
  [2,22,2,66,2,110,2,33,2,77,2,0,2,44,2,88,2,11,2,55,2,99,2,22,2,66,2,110,2,33,2,77,2,0,2,44,2,88,2,11,2]

  > f 26 <$> [0..40] -- 26
  [2,52,2,156,2,260,2,364,2,468,2,572,2,0,2,104,2,208,2,312,2,416,2,520,2,624,2,52,2,156,2,260,2,364,2,468,2,572,2,0,2]

  Note that:
  - when n is even, the result is always 2.
  - 2, n*2, 2, n*2*3 mod n*n, 2, n*2*3*6 mod n*n, 2, ...
  - every number seems to eventually run into a circle.

  For now it looks like for any n, the max r can be found by trying
  just n <- [0..a*2-1].
 -}

f :: Int -> Int -> Int
f a n = (powModInt (a-1) n aSq + powModInt (a+1) n aSq) `rem` aSq
  where
    aSq = a * a

  {- fromIntegral $ ((a'-1) ^! n + (a'+1) ^! n) `rem` (a' * a')
  where
    a' :: Integer
    a' = fromIntegral a -}

findMax a = maximum $ 2 : do
  n <- [0..a*2-1]
  guard $ odd n
  pure $ f a n

result = sum $ findMax <$> [3..1000]
