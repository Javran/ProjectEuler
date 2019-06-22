module ProjectEuler.Problem26
  ( problem
  ) where

import Data.List
import Data.Maybe
import Data.Function (on)

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 26 Solved result

-- TODO
-- 1/n = 0.???? ...
-- this function finds ???? for n
--   by emulating how we do division on draft
--   (quotients and remainders are all kept,
--   so you might need `map fst $ fracPartList n`
fracPartList :: Int -> [(Int,Int)]
fracPartList n = unfoldr doDiv 10
  where
    doDiv x
      | x == 0 = Nothing
      | r == 0  =
          -- this would still work without this case, but
          -- shortcutting allows better speed.
          -- try to keep the first digit
          --   by assigning next state to be a dummy value(0)
        Just ((q,r), 0)
      | otherwise = Just ((q,r),r*10)
      where
        (q,r) = x `quotRem` n

cycleLen :: [Int] -> Int
cycleLen = cycleLenAux []
  where
    cycleLenAux _ [] = 0
    cycleLenAux visited (hd:tl) =
      if hd `elem` visited
        then fromJust (elemIndex hd visited) + 1
        else cycleLenAux (hd:visited) tl

fracCycleLen :: Int -> Int
fracCycleLen n = cycleLen $ snd <$> fracPartList n

result :: Int
result = maximumBy (compare `on` fracCycleLen) [2..999]

