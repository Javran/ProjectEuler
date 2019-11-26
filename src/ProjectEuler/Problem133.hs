{-# LANGUAGE BangPatterns #-}
module ProjectEuler.Problem133
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Primes

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 133 Solved result

divisibleSmall :: Integer -> Bool
divisibleSmall p = not . null $ do
  n <- [1 :: Int .. 200]
  guard $ powMod (10 :: Integer) ((10 :: Integer) ^ n) (9 * p) == 1

{-
  The tricky part is that we don't know how to search for those that will never be a factor,
  so instead we search for those that could be a factor, and see how to go from there.

  The following gives: 11,17,41,73,101,137,251,257,271,353
  which leads me to https://oeis.org/A178070, that sounds like what we want.
 -}
_doSearch :: [Integer]
_doSearch = take 10 $ filter divisibleSmall (fmap unPrime primes)

-- make sure that p > 5 (or, in other words, gcd(p, 10) = 1), otherwise this function will never return.
findMultOrder :: Int -> Int
findMultOrder p = go 1 (10 `rem` p)
  where
    go !k 1 = k
    go !k acc = go (k+1) ((acc * 10) `rem` p)

-- same constraint as findMultOrder, p > 5.
_couldDivide :: Int -> Bool
_couldDivide p = case factorise (findMultOrder p) of
  [(pa, _)] ->
    unPrime pa `elem` [2,5] -- 2^? or 5^?
  [(pa, _), (pb, _)] ->
    -- 2^i * 5^j where i > 0 and j > 0
    -- well, since there's no guarantee on ordering of pa and pb,
    -- the only way to achieve this is through 2+5 or 5+2
    7 == unPrime pa + unPrime pb
  _ -> False

{-
  A slightly more optimized version of _couldDivide, which only attempts to
  divide result of findMultOrder by 2s, and then by 5s.
 -}
couldDivide' :: Int -> Bool
couldDivide' p = go0 kInit
  where
    kInit = findMultOrder p
    go0 k
      | (q,r) <- k `quotRem` 2 = if r == 0 then go0 q else go1 k
    go1 1 = True
    go1 k | (q,r) <- k `quotRem` 5 = if r == 0 then go1 q else False

result :: Int
result = 2 + 3 + 5 + sum
  [ p | p <- takeWhile (< 100000) . dropWhile (<= 5) $ fmap unPrime primes, not . couldDivide' $ p ]
