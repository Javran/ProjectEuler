module ProjectEuler.Problem150
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.Maybe
import Petbox

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 150 Solved result

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

  Note: the change from using Data.Map.Strict to a linear indexed unboxed Vector
  is very siginificant:

  - Data.Map.Strict: 360720.5873 ms
  - Data.Vector.Unboxed: 1341.3732 ms

  TODO: cleanup.

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

type TriSum = VU.Vector Int32

{-
  Given a 2d index (i,j) where i <= j,
  make a 1d index.
 -}
triSumInd :: Int -> Int -> Int
triSumInd i j = i + (j*(j+1) `rem` 2)

mkTriSum :: TriSum -> [Int32] -> TriSum
mkTriSum prevVec curRow = runST $ do
    let sz = triSumInd (len-1) (len-1) + 1
    curVec <- VUM.unsafeNew sz
    forM_ [1 .. len] $ \l ->
      forM_ [0 .. len-l] $ \i -> do
        let j = i + l - 1
            baseSum = getCurRowSum i j
        VUM.write
          curVec
          (triSumInd i j)
          (baseSum + if l == 1 then 0 else prevVec VU.! triSumInd i (j-1))
    VU.unsafeFreeze curVec
  where
    len = length curRow
    getCurRowSum = mkFastSum curRow

theTriangle :: [[Int32]]
theTriangle = unfoldr go (1, gens)
  where
    gens = unfoldr (Just . linearCongruentialGen) 0
    go (n, xs)
      | n > 1000 = Nothing
      | (ys,zs) <- splitAt n xs = Just (ys, (n+1,zs))

result :: Int32
result =
    minimum $ mapMaybe triSumToMin $ scanl mkTriSum (VU.fromList []) theTriangle
  where
    triSumToMin m =
      if VU.length m == 0
        then Nothing
        else Just (minimum (VU.toList m))

linearCongruentialGen :: Int64 -> (Int32, Int64)
linearCongruentialGen t = (s, t')
  where
    -- for powers of 2, we have:
    -- x `rem` (2^k) === x .&. (2^k - 1)
    -- where k >= 0
    t' = (615949*t + 797807) .&. (2 ^! 20 - 1)
    s :: Int32
    s = fInt $ t' - 2 ^! 19
