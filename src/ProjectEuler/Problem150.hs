module ProjectEuler.Problem150
  ( problem
  ) where

import Data.Bits
import Data.Int
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 150 Unsolved result

{-
  Idea:

  Note that all triangles that we want to search can be uniquely represented
  by two coordinates on the same row - these two coordinates will be the base
  of that triangle. Since this triangle is equilateral and we've known
  the length and position of its base, we have everything we need for our task.

  let (r,c) be coordinates in this large triangle, 0 <= r < 1000, 0 <= c <= r.

  Now we can maintain two arrays as we approach the final answer:

  - a rowSums[i] array that records accumulative sum between 0..i of that row,
    for a O(1) range-sum lookup.
  - a triSums[i,j] array, in which i <= j, that records sum of the triangles
    whose base is from i to j of that row. it should be possible to
    reuse triSum of previous row so we never go back to already processed rows
    to do the summation.

 -}

result = take 10 (unfoldr (Just . linearCongruentialGen) 0)

linearCongruentialGen :: Int64 -> (Int32, Int64)
linearCongruentialGen t = (s, t')
  where
    -- for powers of 2, we have:
    -- x `rem` (2^k) === x .&. (2^k - 1)
    -- where k >= 0
    t' = (615949*t + 797807) .&. (2 ^! 20 - 1)
    s :: Int32
    s = fInt $ t' - 2 ^! 19
