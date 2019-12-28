module ProjectEuler.Problem150
  ( problem
  ) where

import Data.Bits
import Data.Int
import Petbox
import Data.Maybe

import qualified Data.Map.Strict as M
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

mkTriSum :: M.Map (Int,Int) Int32 -> [Int32] -> M.Map (Int,Int) Int32
mkTriSum prevRow curRow = M.fromList $ do
    l <- [1 .. len]
    i <- [0 .. len - l]
    let j = i + l - 1
    if l == 1
      then pure ((i,j), getCurRowSum i i)
      else
        let baseSum = getCurRowSum i j
        in pure ((i,j), baseSum + prevRow M.! (i,j-1))
  where
    len = length curRow
    getCurRowSum = mkFastSum curRow

sample :: [[Int32]]
sample =
  [ [15]
  , [-14,-7]
  , [20,-13,-5]
  , [-3,8,23,-26]
  , [1,-4,-5,-18,5]
  , [-16,31,2,9,28,3]
  ]

theTriangle :: [[Int32]]
theTriangle = unfoldr go (0, gens)
  where
    gens = unfoldr (Just . linearCongruentialGen) 1
    go (1001, _) = Nothing
    go (n, xs) = Just (ys, (n+1,zs))
      where
        (ys,zs) = splitAt n xs

result =
    minimum $ mapMaybe triSumToMin $ scanl mkTriSum M.empty theTriangle
  where
    triSumToMin m =
      if M.size m == 0
        then Nothing
        else Just (minimum (M.elems m))

linearCongruentialGen :: Int64 -> (Int32, Int64)
linearCongruentialGen t = (s, t')
  where
    -- for powers of 2, we have:
    -- x `rem` (2^k) === x .&. (2^k - 1)
    -- where k >= 0
    t' = (615949*t + 797807) .&. (2 ^! 20 - 1)
    s :: Int32
    s = fInt $ t' - 2 ^! 19
