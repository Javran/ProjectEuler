module ProjectEuler.Problem105
  ( problem
  ) where

import Control.Monad
import Data.Bits
import Data.List
import Data.Monoid

import qualified Data.IntSet as IS
import qualified Data.Text as T

import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p105_sets.txt" 105 Solved compute

pickInOrder :: [a] -> [] (a,[a])
pickInOrder = fmap (\(x:xs) -> (x,xs)) . init . tails

isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

{-
  TODO:
  we ported from Problem102, but at some point in time we should
  share code for this.
 -}

solve :: Int -> [Int] -> Int -> [Int] -> [IS.IntSet] -> [[Int]]
solve solSz curList sz candidates sumSets
  | sz == solSz = pure (reverse curList)
  | otherwise = do
      (x,candidates') <- pickInOrder candidates
      let sumSetsWithX =
            IS.fromDistinctAscList . fmap (+x) . IS.toAscList <$> sumSets
          sumSets' = zipWith IS.union (sumSets <> [IS.empty]) (IS.empty : sumSetsWithX)
          allSums' = foldl' IS.union IS.empty sumSets'
          getMinMax s
            | IS.null s = []
            | IS.size s == 1 = [IS.findMin s]
            | otherwise = [IS.findMin s, IS.findMax s]
      guard $ IS.size allSums' == 1 `unsafeShiftL` (sz+1)
      guard $ isIncreasing (foldMap getMinMax sumSets')
      solve solSz (x:curList) (sz+1) candidates' sumSets'

getSpecialSubsetSum :: [Int] -> Sum Int
getSpecialSubsetSum xs = case solve l [] 0 xs [IS.singleton 0] of
    [] -> 0
    x:_ -> Sum $ sum x
  where
    l = length xs

compute :: T.Text -> Int
compute =
    getSum
    . foldMap (getSpecialSubsetSum . parseLine)
    . lines
    . T.unpack
  where
    parseLine raw = read ("[" <> raw <> "]") :: [Int]

