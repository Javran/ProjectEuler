{-# LANGUAGE NoMonomorphismRestriction #-}
module ProjectEuler.Problem149
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Int
import Petbox
import Data.List

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 149 Solved result

{-
  Idea:

  I don't think this is a particularly large array to build,
  but it might improve performance if we just compute the value
  on the fly knowing the coordinate.
  Then rest of it is just generic algorithm for figuring out max adjacent sum
  (with negative elements taking into account)

  might be helpful: https://en.wikipedia.org/wiki/Lagged_Fibonacci_generator
  perhaps, given that it is designed for generating random numbers,
  we aren't supposed to find any patterns that can help speeding up the process.
  but I'll need to take a closer look.

  Assuming that we can learn nothing of use from the way that these numbers are generated,
  we can still use Kadane's algorithm as describe from:
  https://en.wikipedia.org/wiki/Maximum_subarray_problem

  Afterthoughts: (TODO)

  Note that it is not necessary to have the full matrix available before running the algorithm:
  for every coordinate (r,c), we send the value in question to:
  - an array indexed by r
  - another array indexed by c
  - another array indexed by r+c
  - another array indexed by r-c

  then after we have gone through all the numbers, we can simply scan these 4 arrays
  to get the max out of them.

 -}

result :: Int32
result = maximum (findMax numTable 2000 (genLineCoords 2000))

type Coord = (Int,Int)
type LineCoords = [] Coord -- coordinates that are in the same line.

genLineCoords :: Int -> [] LineCoords
genLineCoords l =
    {-
      note that for coordinate (r,c), we have 1 <= r,c <= l,
      the index is started at one to make it consistent with problem's description.
     -}
    rows <> cols <> diags0 <> diags1
  where
    rows = [ [(r,c) | c <- [1..l]] | r <- [1..l] ]
    cols = [ [(r,c) | r <- [1..l]] | c <- [1..l] ]
    -- let's say diags0 is of the direction that keeps r + c = k a constant.
    diags0 =
      [ [ (r,c) | r <- [1..k-1], let c = k - r ] | k <- [2 .. l+1] ]
      <> [ [ (r,c) | r <- [k-l..l], let c = k - r ] | k <- [l+2 .. l+l] ]
    -- diags1 keeps r - c = k a constant. (TODO)
    diags1 =
      [ [ (r,c) | r <- [1 .. l+k], let c = r - k ] | k <- [1-l .. 0]]
      <> [ [ (r,c) | r <- [k+1 .. l] , let c = r - k] | k <- [1 .. l-1] ]

getVal :: VUM.Unbox a => VU.Vector a -> Int -> Coord -> a
getVal vec l (r,c) = vec VU.! ((r-1)*l + c)

findMax :: VU.Vector Int32 -> Int -> [LineCoords] -> [Int32]
findMax vec l = fmap (maxSubArray . getVecLine)
  where
    getVecLine :: [(Int,Int)] -> [Int32]
    getVecLine = fmap (getVal vec l)

{-
  Kadane's algorithm to compute sum of adjacent numbers.
  Note that if the array (or the sequence of things) are all negative,
  the algorithm will return 0, which is fine for our case, because
  there are definitely positive numbers in our array (for example, s_100 = 86613)
 -}
maxSubArray :: (Foldable t, Num a, Ord a) => t a -> a
maxSubArray = fst . foldl' kadaneAux (0, 0)
  where
    kadaneAux (bestSum, prevSum) curVal = (bestSum', curSum)
      where
        curSum = max 0 (prevSum + curVal)
        bestSum' = max bestSum curSum

numTable :: VU.Vector Int32
numTable = runST $ do
    vec <- VUM.unsafeNew sz
    -- for 1 <= k <= 55
    forM_ [1..55] $ \k -> do
      let -- being careful here not to overflow.
          v0 = 100003 - 200003 * k
          v1 = modMul (k*k) (modMul k 300007)
          val = modPlus v0 v1 - 500000
      VUM.write vec k (fromIntegral val)
    forM_ [56..l*l] $ \k -> do
      kM24 <- VUM.read vec (k-24)
      kM55 <- VUM.read vec (k-55)
      let v0 :: Int
          v0 = modPlus (modPlus (fInt kM24) (fInt kM55)) 1000000
          val = v0 - 500000
      VUM.write vec k (fromIntegral val)
    VU.unsafeFreeze vec
  where
    -- `rem` and `mod` produces the same result when m is non-negative,
    -- but rem is slightly more efficient to use.
    modPlus a b = (a + b) `rem` m
    modMul a b = (a * b) `rem` m
    m = 1000000
    l = 2000
    sz = 1 + l * l
