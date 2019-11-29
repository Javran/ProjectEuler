module ProjectEuler.Problem135
  ( problem
  , countSameDiffs
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.Word

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 135 Solved result

{-
  Let's get some symbol pushing going:

  (m+d)^2 - m^2 - (m-d)^2 = n > 0.

  w.l.o.g.: m > d > 0.
  this also implies that m > 1, we don't have a value for d otherwise.

  (m+d)^2 - m^2 - (m-d)^2
  = (m+d+m-d)(m+d-m+d) - m^2
  = 4 m d - m^2
  = (4 d - m) * m = n > 0

  so this gives us another constraint:

  4 d - m > 0 => m < 4 d,

  so for a fixed value n, m can be one of its divisor.

  d = (n + m^2) / (4 * m) must be an integer.

  If we further plug in d < m:

  (n + m^2) / 4 * m < m
  >  n < 3 * m^2

  This gives us a tighter range on m.
  Also note that since d can be expressed directly using only m and n,
  we only need to search for m and verify that d is valid.
  Let's try this idea out.

 -}

countSameDiffs :: Int -> VU.Vector Word16
countSameDiffs maxN = runST $ do
  vec <- VUM.replicate (maxN+1) (0 :: Word16)
  forM_ [2 .. maxN] $ \m -> do
    {-
      Since demanding that d < m is the same as demanding n < 3 * m^2,
      we might as well do this in the first place and don't bother checking
      d < m.
     -}
    let maxN' = min maxN (3 * m * m-1)
    forM_ [m, m+m .. maxN'] $ \n -> do
      let numer = n + m * m
          denom = 4 * m
      when (numer `rem` denom == 0) $
        VUM.modify vec succ n
  VU.unsafeFreeze vec

result :: Int
result =
  getSum
  . foldMap (\n -> if n == 10 then 1 else 0)
  . VU.toList
  $ countSameDiffs (1000000-1)

