module ProjectEuler.Problem136
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.Word
import Petbox

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
  - d < m, or equivalently n < 3 * m^2

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
  n = 4, 16, p === 3 (mod 4), 4*p, 16*p are all the solutions (p > 2 therefore is odd)
  (TODO: proof pending)

  note: maxN > 16
 -}
countSameDiffs :: Int -> Int
countSameDiffs maxN = 2 + case1 + case2 + case3
  where
    oddPrimes = takeWhile (<= maxN) $ tail primes
    case1 =
      getSum
      . foldMap (\v -> if v `rem` 4 == 3 then 1 else 0)
      $ oddPrimes
    case2 = length $ takeWhile (<= (maxN `quot` 4)) oddPrimes
    case3 = length $ takeWhile (<= (maxN `quot` 16)) oddPrimes

result :: Int
result = countSameDiffs (50000000-1)
