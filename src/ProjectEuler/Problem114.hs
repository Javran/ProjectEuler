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
  Let f(i,j) be the ways to occupy range i to j (inclusive)
  with position i and position j taken as edge of a block.

  We can grow the length between i and j to eventually cover
  whole set. Also beware that not putting any block at all counts
  as a valid solution, so we'll need to have some special handlings.

  Update: no clue on this one - len-based dynamic programming
  doesn't break down into sub-problems well:
  we'll have to deal with duplicated case given the definition of f.

  Alternative plan:

  let f(i) be the ways to occupy from 1 to i (inclusive),
  and all these ways has to end at i. - let's see if this definition gets us anywhere.

  therefore:

  f(i) = 0 (i < 3)
  f(3) = 1 [XXX]
  f(4) = 2 [XXXX] or [_XXX]
  f(5) = 3 [XXXXX] or [_XXXX] or [__XXX]

  I guess there is two ways that we can construct a solution for f(i):

  - from f(i-1), simply extend the last block by 1
  - from f(j) where j < i and there's enough space to add another block of length (of at least 3),
    and this another block must end at position i.

  formalize this:

  f(i) = f(i-1) + extra, where extra are the sets of solutions constructed by putting
  one extra block who ends at i:
  - [j+2 .. i]
  - [j+3 .. i]
  - ...
  - [i-2 .. i]

  => f(i) = f(i-1) + sum of { f(j) * (i-j-3), for all i - j > 3 } + 1

  the last (+1) term is for a 3-block in the end, which cannot be derived from f(i-1)


  if this works, we'll get sum of { f(i) } where i <= 7 equal to 16,
  which one extra "nothing at all" solution to get a sum of 17.

 -}

f :: Int -> State (IM.IntMap Integer) Integer
f i
  | i < 3 = pure 0
  | otherwise =
      -- 1 + f (i-1) + sum ((\j -> f j * (i-j-3)) <$> [0,1..i-4])
      gets (IM.lookup i) >>= \case
        Nothing -> do
          v1 <- f (i-1)
          ts <- forM [0,1..i-4] $ \j -> do
            vz <- f j
            pure $ vz * fromIntegral (i-j-3)
          let r = 1 + v1 + sum ts :: Integer
          modify (IM.insert i r)
          pure r
        Just v -> pure (v :: Integer)

result :: Integer
result = 1 + sum (evalState (mapM f [1..7]) IM.empty)
