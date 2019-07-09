module ProjectEuler.Problem95
  ( problem
  ) where

import Data.List
import Data.Ord
import Data.Word
import Control.Monad
import Control.Monad.ST

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 95 Solved result

{-

  Some ideas:
  - https://en.wikipedia.org/wiki/Sociable_number
  - https://en.wikipedia.org/wiki/Aliquot_sequence

  Since we want to find chains of values formed by Aliquot sequence,
  we can first compute (x, sumOfProperDivisors x) for all values in the search space,
  then we restrict keys by collecting all values of `sumOfProperDivisors` (cutting),
  and doing this repeatly will eventually get us to a point
  that all remaining values of this map is in some cycle and no more cutting
  can be made.

  Fortunately with 20 seconds, only 117 pairs are remaining,
  which is far more managable to search through.

  Update: since we are working with a larget number of values,
  whose sum of proper divisors are all needed, we can avoid
  dealing with each individual ones and just work with an vector to
  sort it out - it's actually way faster.

  TODO: we don't really need multiple rounds of reduction to get to the
  "cycle nodes only" point - the vector should be fast enough to deal with.

 -}

maxN :: Int
maxN = 1000000

sumOfProperDivisorsVec :: V.Vector Word32
sumOfProperDivisorsVec = runST $ do
  vec <- VM.replicate (maxN+1) 1
  VM.write vec 0 0
  VM.write vec 1 0
  forM_ [2..fromIntegral maxN] $ \i ->
    forM_ [i+i,i+i+i..maxN] $ \j ->
      VM.modify vec (+ fromIntegral i) j
  V.unsafeFreeze vec

loopMapInit :: IM.IntMap Int
loopMapInit = cutClear $ IM.fromList pairs
  where
    pairs = V.ifoldl' go [] sumOfProperDivisorsVec
      where
        go :: [(Int,Int)] -> Int -> Word32 -> [(Int,Int)]
        go xs i val
          | i < 2 || val < 2 || val > fromIntegral maxN = xs
          | otherwise =
            let v = fromIntegral val
            in (v, fromIntegral $ V.unsafeIndex sumOfProperDivisorsVec v) : xs

cut :: IM.IntMap Int -> IM.IntMap Int
cut m = IM.restrictKeys m vals
  where
    vals = IS.fromList $ IM.elems m

cutClear :: IM.IntMap Int -> IM.IntMap Int
cutClear m =
  if IM.size m == IM.size m'
    then m'
    else cutClear m'
  where
    m' = cut m

extractLoop :: IM.IntMap Int -> (IS.IntSet, IM.IntMap Int)
extractLoop m = case IM.minViewWithKey m of
    Nothing -> (IS.empty, m)
    Just ((k,_), _) ->
      let findLoop start cur acc =
            let next = m IM.! cur
            in if next == start
                then acc
                else findLoop start next (IS.insert cur acc)
          loop = findLoop k k (IS.singleton k)
      in (loop, cutClear (IM.withoutKeys m loop))

result :: Int
result = IS.findMin $ head sortedLoopGroups
  where
    -- sort by descending group size to find the maximum
    sortedLoopGroups = sortOn (Down . IS.size) loopGroups
    loopGroups :: [IS.IntSet]
    loopGroups = unfoldr go loopMapInit
    go loopMap =
      if IS.null grp
        then Nothing
        else Just (grp, loopMap')
      where
        (grp, loopMap') = extractLoop loopMap
