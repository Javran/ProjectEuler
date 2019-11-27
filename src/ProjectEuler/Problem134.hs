module ProjectEuler.Problem134
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 134 Unsolved result

primePairs :: [(Integer, Integer)]
primePairs = zip ps (tail ps)
  where
    ps = takeWhile (<= 1000000) $ dropWhile (< 5) primes

findConn :: Integer -> Integer -> Integer
findConn p1 p2 = go (p2 * floor (fromIntegral (p1+mask) / fromIntegral p2 :: Double))
  where
    l = digitLen p1
    mask :: Integer
    mask = 10 ^! l
    go cur =
      if cur `rem` mask == p1
        then cur
        else go (cur + p2)

{-
  Let's try the obvious and see how it goes.
 -}

result = sum $ uncurry findConn <$> primePairs
