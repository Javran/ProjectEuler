{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor #-}
module ProjectEuler.Problem149
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Int
import Petbox

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
result = findMatrixMax 2000 cells

type Coord = (Int,Int)

{-
  Two vectors of the same length for running Kadane's algorithm.
  - First vector holds current best value.
  - Second vector holds current sum.
 -}
data Kadane v = Kadane v v deriving Functor

{-
  Kadane's algorithm to compute sum of adjacent numbers.
  Note that if the array (or the sequence of things) are all negative,
  the algorithm will return 0, which is fine for our case, because
  there are definitely positive numbers in our array (for example, s_100 = 86613)
 -}
kadaneAux :: (Num a, Ord a) => (a, a) -> a -> (a, a)
kadaneAux (bestSum, prevSum) curVal = (bestSum', curSum)
  where
    curSum = max 0 (prevSum + curVal)
    bestSum' = max bestSum curSum

findMatrixMax :: Int -> [] (Coord, Int32) -> Int32
findMatrixMax l cs = runST $ do
  let initKadane sz = Kadane <$> VUM.new sz <*> VUM.new sz
      updateKadane (Kadane vecBest vecCur) i val = do
        vBest <- VUM.read vecBest i
        vCur <- VUM.read vecCur i
        let (vBest', vCur') = kadaneAux (vBest, vCur) val
        VUM.write vecBest i vBest'
        VUM.write vecCur i vCur'
  rowsKn <- initKadane l
  colsKn <- initKadane l
  diags0Kn <- initKadane (l+l-1)
  diags1Kn <- initKadane (l+l-1)
  let base = l-1 -- this base allows diags1Kn's index to start from 0.
  forM_ cs $ \((r,c), val) -> do
    updateKadane rowsKn r val
    updateKadane colsKn c val
    updateKadane diags0Kn (r+c) val
    updateKadane diags1Kn (r-c+base) val
    pure ()
  let getMax (Kadane mvec _) = do
        vec <- VU.unsafeFreeze mvec
        pure (maximum (VU.toList vec))
  rowsMax <- getMax rowsKn
  colsMax <- getMax colsKn
  diags0Max <- getMax diags0Kn
  diags1Max <- getMax diags1Kn
  pure $ maximum [rowsMax, colsMax, diags0Max, diags1Max]

cells :: [] (Coord, Int32)
cells = zip [ (r,c) | r <- [0..1999], c <- [0..1999] ] vals
  where
    _ : vals = VU.toList numTable

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
