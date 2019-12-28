module ProjectEuler.Problem150
  ( problem
  ) where

import Data.Bits
import Data.Int
import Petbox

import qualified Data.Vector.Unboxed as VU

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 150 Unsolved result

{-
  Idea:

  Note that all triangles that we want to search can be uniquely represented
  by two coordinates on the same row - these two coordinates will be the base
  of that triangle. Since this triangle is equilateral and we've known
  the length and position of its base, we have everything we need for our task.

  - let (r,c) be coordinates in this large triangle, 0 <= r < 1000, 0 <= c <= r.
  - let a(r,c) be the value located at (r,c)
  - let rowSum(r,c0,c1) be the summation from (r,c0) to (r,c1) where c0 <= c1.
  - let triSum(r,c0,c1) be the summation of the triangle whose base lies
    between (r,c0) and (r,c1) inclusively. (where c0 <= c1)

    triSum(r,c,c) = a(r,c)
    triSum(r,c0,c1) = rowSum(r,c0,c1) + triSum(r-1,c0,c1-1)

  Now our task is simply to find the minimum of triSum.

 -}

{-
  preprocess the input array to return a function getSum,
  where getSum i j = sum of elements from i to j (inclusive), require i <= j.
 -}
mkFastSum :: [Int32] -> (Int -> Int -> Int32)
mkFastSum xs = getSum
  where
    getSum i j = (vs VU.! (j+1)) - (vs VU.! i)
    l = length xs
    vs = VU.fromListN (l + 1) $ 0 : scanl1 (+) xs

result =
  take 10 (unfoldr (Just . linearCongruentialGen) 0)

linearCongruentialGen :: Int64 -> (Int32, Int64)
linearCongruentialGen t = (s, t')
  where
    -- for powers of 2, we have:
    -- x `rem` (2^k) === x .&. (2^k - 1)
    -- where k >= 0
    t' = (615949*t + 797807) .&. (2 ^! 20 - 1)
    s :: Int32
    s = fInt $ t' - 2 ^! 19
