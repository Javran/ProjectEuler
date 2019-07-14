module ProjectEuler.Problem103
  ( problem
  ) where

import Control.Monad
import Data.List
import Data.Bits
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 103 Solved result

{-
  First guess: looking at last number of each optimum set:

  > 1,2,4,7,13,25

  From the looks of it we may want to assume the growth is slightly
  less than 2 at each step, so let's assume maximum number is 50 and
  walk our way down.

 -}

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
pickInOrder :: [a] -> [] (a,[a])
pickInOrder = fmap (\(x:xs) -> (x,xs)) . init . tails

isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

{-
  - candidates: list of not yet used candidates
  - sumSets: list of sum set:
    + sumSets !! 0 => always a singleton set {0}
    + sumSets !! 1 => set of sum of subsets of size 1
    + sumSets !! 2 => set of sum of subsets of size 2
    + etc.
 -}
solve :: [Int] -> Int -> [Int] -> [IS.IntSet] -> [[Int]]
solve curList sz candidates sumSets
  | sz == 7 = pure (reverse curList)
  | otherwise = do
      (x,candidates') <- pickInOrder candidates
      let sumSetsWithX =
            -- since we know this map is strictly monotonic,
            -- we can bypass checks by reconstructing altogether.
            IS.fromDistinctAscList . fmap (+x) . IS.toAscList <$> sumSets
          sumSets' = zipWith IS.union (sumSets <> [IS.empty]) (IS.empty : sumSetsWithX)
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

{-
  Note 1: but listing candidates as [50,49...],
  we managed to get one solution: [26,37,43,46,48,49,50],
  so at least we have found an upper bound of the final solution.

  Note 2: through trial-and-error (if program doesn't give an answer
  in few seconds, perhaps we don't really have a solution),
  we find that [46,45...] gives [22,33,39,42,44,45,46],
  while [45,44..] take forever - I'd say that's a closer upperbound.

  Note 3: now let's turn it around and use candidate list [1..46].
  The reason for this is that:
  we already found a close upperbound for the max number,
  and want to find the first solution, which got to be the optimum.
 -}

result :: Int
result =
  read . concatMap show . head
  $ solve [] 0 [1..46] [IS.singleton 0]
