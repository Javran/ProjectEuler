module ProjectEuler.Problem136
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.Word

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 136 Solved result

{-
  Using same method as in Problem135 works,
  but it is a bit slower than I like.

  There are various ways that we can do a better job based on the brute force
  we established in Problem135,
  but I don't think we can have any significant improvement along that path.

  The following is mostly done after I've solved the problem,
  to both convince myself and whoever reading this that a solution
  better than brute force exists.

  Carrying over from Problem135:

  (m+d)^2 - m^2 - (m-d)^2 = n > 0.

  w.l.o.g.: m > d > 0

  - m > 1.
  - (m+d)^2 - m^2 - (m-d)^2 = (4 d - m) * m = n > 0

  And for Problem136, we want to explore:
  "when is the solution (m,d) unique, given n?"

  Let's focus on `n = (4d - m) * m` for now:

  - let v = 4d - m, we have n = v * m && m + v = 4d
    (N.B. I'm not sure why we define v in the first place,
    my guess is that it makes the writing a bit easier)
  - here the idea is to explore all pairs (v, m)
    and find those m + v = 4d and check their validities.
  - let n = 2^u * r, where u >= 0 and r is an odd number.
    In other words, 2^u is all the "even-ness" of n.

  TODO: finish this.

 -}

{-
  Note the difference between this one and the one in Problem135:
  for this one we are only interested in those that has exactly one
  solution, therefore we can stop when we have a counting more than one,
  this cuts out memory consumption as the value we store for each element
  is at most 2.
 -}
countSameDiffs :: Int -> VU.Vector Word8
countSameDiffs maxN = runST $ do
  vec <- VUM.replicate (maxN+1) (0 :: Word8)
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
      when (numer `rem` denom == 0) $ do
        v <- VUM.read vec n
        when (v < 2) $
          VUM.write vec n (v+1)
  VU.unsafeFreeze vec

result :: Int
result =
  getSum
  . foldMap (\n -> if n == 1 then 1 else 0)
  . VU.toList
  $ countSameDiffs (50000000-1)

