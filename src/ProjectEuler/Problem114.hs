{-# LANGUAGE
    LambdaCase
  #-}
module ProjectEuler.Problem114
  ( problem
  ) where

import Control.Monad.State
import qualified Data.IntMap as IM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 114 Unsolved result

{-
  Idea: perhaps dynamic programming is possible on this one:

  Let f(i) be the ways to occupy from 1 to i (inclusive),
  and all these ways has to:

  - at least have one block in it.
  - the last block must end at position i.

  therefore:

  f(i) = 0 (i < 3)
  f(3) = 1 [XXX]
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
  and the missing case corresponds to "no block at all".

  To avoid counting duplicated cases, let's consider
  the final block being of length l:
  (from problem description we know l >= 3)

  - note that for l = 3 to l = i, we can do a "nothing but last block" case,
    which accounts for i-3+1 = i-2 cases.
  - besides the case above, for 3 <= j <= i-l-1, we can sum up f(j) for  3 <= l <= i

 -}

{-

f i
  | i < 3 = 0
  | i == 3 = 1
  | otherwise =
      let g :: Int -> Int
          g l = sum (f <$> [3,4..i-l-1])
      in (i-2) + sum (g <$> [3,4..i])
 -}

f :: Int -> State (IM.IntMap Integer) Integer
f i
  | i < 3 = pure 0
  | i == 3 = pure 1
  | otherwise =
      gets (IM.lookup i) >>= \case
        Nothing -> do
          let g :: Int -> State (IM.IntMap Integer) Integer
              g l = do
                xs <- mapM f [3,4..i-l-1]
                pure (sum xs)
          ys <- mapM g [3,4..i]
          let r = fromIntegral (i-2) + sum ys
          modify (IM.insert i r)
          pure r
        Just v -> pure (v :: Integer)

result :: Integer
result = 1 + sum (evalState (mapM f [1..50]) IM.empty)
