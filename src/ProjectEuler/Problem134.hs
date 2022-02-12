module ProjectEuler.Problem134
  ( problem
  )
where

import Math.NumberTheory.Moduli
import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 134 Solved result

{-
  The obvious way: enumerate p2 * k (k = 1, 2, ..) until we get
  a value ending with p1. This approach is too slow to be qualified
  as a "under one minute" solution. We must do something better.

  Let's do this properly this time:

  let l be the number of digits p1 has in decimal without any leading zeros
  (i.e. l = digitLen p1), now we want to find:

  p1 + 10^l * m == p2 * n == N

  Suggesting that we could try Chinese remainder:

  we want to find N such that:

  N `mod` 10^l == p1
  N `mod` p2 == 0

  Indeed this gets us to the solution in no time.

 -}

{-
  Note: don't do dropWhile + takeWhile first before making the pair,
  instead, generate the pair and then dropWhile + takeWhile on the resulting value.

  This is because otherwise while we want constraints on p1 s.t. <= 1000000,
  p2 will also unnecessarily constrainted by this,
  resulting in the last pair (999983,1000003) being missing.
 -}
primePairs :: [(Int, Int)]
primePairs =
  takeWhile ((<= 1000000) . fst)
    . dropWhile ((< 5) . fst)
    $ zip primes (tail primes)

findConn :: Int -> Int -> Int
findConn p1 p2 = case r of
  SomeMod k -> fromInteger $ getVal k
  _ -> error "unreachabke"
  where
    l = digitLen p1
    Just r = chineseSomeMod (fromIntegral p1 `modulo` (10 ^! l)) (0 `modulo` fromIntegral p2)

result :: Int
result = sum $ uncurry findConn <$> primePairs
