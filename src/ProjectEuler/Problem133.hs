module ProjectEuler.Problem133
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Modular
import Data.List
import Control.Monad

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

findMultOrder :: Int -> Int
findMultOrder p = go 1 (10 `rem` p)
  where
    go k 1 = k
    go k acc = go (k+1) ((acc * 10) `rem` p)

-- make sure that p > 5 (or, in other words, gcd(p, 10) = 1), otherwise this function will never return.
couldDivide :: Int -> Bool
couldDivide p = case factorise (findMultOrder p) of
  [(pa, _)] ->
    unPrime pa `elem` [2,5] -- 2^? or 5^?
  [(pa, _), (pb, _)] ->
    null ([2,5] \\ [unPrime pa, unPrime pb]) -- 2^i * 5^j where i > 0 and j > 0
  _ -> False

result :: Int
result = 2 + 3 + 5 + sum
  [ p | p <- takeWhile (< 100000) . dropWhile (<= 5) $ fmap unPrime primes, not . couldDivide $ p ]

