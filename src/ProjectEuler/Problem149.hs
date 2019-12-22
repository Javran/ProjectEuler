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
problem = pureProblem 149 Unsolved result

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

 -}

result = (numTable VU.! 10, numTable VU.! 100)

numTable :: VU.Vector Int32
numTable = runST $ do
    vec <- VUM.unsafeNew sz
    -- for 1 <= k <= 55
    forM_ [1..55] $ \k -> do
      let -- being careful here not to overflow.
          v0 = modPlus 100003 (modMul (-200003) k)
          v1 = modMul k (modMul k (modMul k 300007))
          val = modPlus v0 v1 - 500000
      VUM.write vec k (fromIntegral val)
    forM_ [56..l*l] $ \k -> do
      kM24 <- VUM.read vec (k-24)
      kM55 <- VUM.read vec (k-55)
      let v0 = modPlus (modPlus (fInt kM24) (fInt kM55)) 1000000
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
