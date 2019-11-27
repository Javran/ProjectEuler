module ProjectEuler.Problem134
  ( problem
  ) where

import Petbox
import Data.Word

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 134 Unsolved result

{-
  Note: don't do dropWhile + takeWhile first before making the pair,
  instead, generate the pair and then dropWhile + takeWhile on the resulting value.

  This is because otherwise while we want constraints on p1 s.t. <= 1000000,
  p2 will also unnecessarily constrainted by this,
  resulting in the last pair (999983,1000003) being missing.
 -}
primePairs :: [(Word64, Word64)]
primePairs =
    takeWhile ((<= 1000000) . fst)
    . dropWhile ((< 5) . fst)
    $ zip primes (tail primes)

{-
  This version is too slow.
 -}
findConn :: Word64 -> Word64 -> Word64
findConn p1 p2 = go (p2 * floor (fromIntegral (p1+mask) / fromIntegral p2 :: Double))
  where
    l = digitLen p1
    mask :: Word64
    mask = 10 ^! l
    go cur =
      if cur `rem` mask == p1
        then cur
        else go (cur + p2)

{-
  Let's try the obvious and see how it goes.
 -}
result :: Word64
result = sum $ uncurry findConn <$> primePairs
