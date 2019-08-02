{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem74
  ( problem
  ) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Petbox

import qualified Data.IntMap.Strict as IM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 74 Solved result

digitFac :: Int -> Int
digitFac =
  getSum
  . foldMap (Sum . factorial)
    {-
      we are using intToDigitsRev here as it's slightly more efficient
      than intToDigits (without reversing) and we don't care about the order
      in this occasion.
     -}
  . intToDigitsRev

-- take combinations of 0!, 1!, ... 9!, 6 times.
-- note that since 0! = 1, we need to remove prefixing zeros before
-- taking the sum
searchSpace :: IM.IntMap Int
searchSpace = IM.fromListWith (+) (map ((,1) . takeSum) (replicateM 6 [0..9]))
  where
    takeSum = sum . map factorial . dropWhile (== 0)

-- loop must exist, no need for Maybe
-- modified from Problem 64
-- returns (<the loop>, <reversed non-repeating sequence>)
findLoop :: Ord a => [a] -> ([a],[a])
findLoop = findLoop' []
  where
    findLoop' _ [] = error "no loop detected"
    findLoop' s (y:ys)
      | y `elem` s = (dropWhile (/= y) . reverse $ s, s)
      | otherwise  = findLoop' (y:s) ys

result :: Int
result = sum $ mapMaybe (`IM.lookup` searchSpace) sndInChains
  where
    valid n = (length . snd . findLoop $ iterate digitFac n) + 1 == 60
    -- the second element in chain
    sndInChains = filter valid $ IM.keys searchSpace
