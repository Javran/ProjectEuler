module ProjectEuler.Problem105
  ( problem
  , findSpecialSubsets
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

{-
  Note: "findSpecialSubsets" together with auxiliary functions
  are originally implemented in ProjectEuler.Problem103,
  they are then moved here and backported back to Problem103
  because this version is more general than one used in Problem103.
 -}

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
{-# INLINABLE pickInOrder #-}
pickInOrder :: [a] -> [] (a,[a])
pickInOrder = fmap (\(x:xs) -> (x,xs)) . init . tails

{-# INLINABLE isIncreasing #-}
isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

{-# INLINABLE findSpecialSubsets #-}
findSpecialSubsets :: Int -> [Int] -> [[Int]]
findSpecialSubsets solSz initCandidates =
    solve [] 0 initCandidates [IS.singleton 0]
  where
    {-
      - candidates: list of not yet used candidates
      - sumSets: list of sum set:
        + sumSets !! 0 => always a singleton set {0}
        + sumSets !! 1 => set of sum of subsets of size 1
        + sumSets !! 2 => set of sum of subsets of size 2
        + etc.

      TODO: Note that I'm not actually interpreting the first property correctly,
      as we only need to check for *disjoint pair of subsets*.
      However this misinterpretation doesn't affect correctness:
      If common elements are present for two subsets, by removing those elements
      from both sets, the net effect is just removing a constant from both sides,
      which has no effect because we only care about whether the sum is distinct.
     -}
    solve :: [Int] -> Int -> [Int] -> [IS.IntSet] -> [[Int]]
    solve curList sz candidates sumSets
      | sz == solSz = pure (reverse curList)
      | otherwise = do
          (x,candidates') <- pickInOrder candidates
          let sumSetsWithX =
                -- since we know this map is strictly monotonic,
                -- we can bypass checks by reconstructing altogether.
                IS.fromDistinctAscList . fmap (+x) . IS.toAscList
                  <$> sumSets
              sumSets' =
                zipWith
                  IS.union
                  (sumSets <> [IS.empty])
                  (IS.empty : sumSetsWithX)
              allSums' = foldl' IS.union IS.empty sumSets'
              getMinMax s
                | IS.null s = []
                | IS.size s == 1 = [IS.findMin s]
                | otherwise = [IS.findMin s, IS.findMax s]

          -- a note about a simple trick:
          -- most of the time it's faster to write down guards
          -- separately rather than combining them with `&&`.
          -- I'm not sure why this is the case though,
          -- for now my theory is that `&&`  does has some extra cost
          -- of doing pattern matching before return value to `guard`,
          -- but I'm not convinced by my own theory and believe ghc should
          -- take care of nested pattern matching (one in `&&` and one in `guard`).

          -- to make sure that all sums are unique, we can simply verify that
          -- the # of elements in list is expected.
          guard $ IS.size allSums' == 1 `unsafeShiftL` (sz+1)
          -- for the size vs sum condition to hold, we just want to know
          -- if we were to extra min and max from each element of sumSets'
          -- (of course for singleton sets we only need one element)
          -- do we end up with an increasing sequence
          -- or, in other words, this is the same as asking that
          -- the range of each pair of neighborhood elements in allSums'
          -- shouldn't overlap.
          guard $ isIncreasing (foldMap getMinMax sumSets')
          solve (x:curList) (sz+1) candidates' sumSets'

getSpecialSubsetSum :: [Int] -> Sum Int
getSpecialSubsetSum xs =
  case findSpecialSubsets (length xs) xs of
    [] -> 0
    x:_ -> Sum $ sum x

compute :: T.Text -> Int
compute =
    getSum
    . foldMap (getSpecialSubsetSum . parseLine)
    . lines
    . T.unpack
  where
    parseLine raw = read ("[" <> raw <> "]")

