module ProjectEuler.Problem101
  ( problem
  ) where

import Data.List
import Data.Ratio

import ProjectEuler.Types

fInt :: Int -> Integer
fInt = fromIntegral

problem :: Problem
problem = pureProblem 101 Unsolved result

u :: Int -> Rational
u n = fromInteger $ sum $ take 11 $ zipWith (*) (cycle [1,-1]) $ iterate (* n') 1
  where
    n' :: Integer
    n' = fInt n

-- TODO: potential for SolCommon
pick :: [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"

{-
  Lagrange polynomial: https://en.wikipedia.org/wiki/Lagrange_polynomial
 -}
-- 1 − n + n^2 − n^3 + n^4 − n^5 + n^6 − n^7 + n^8 − n^9 + n^10
lagrangePoly :: [(Int,Int)] -> Int -> Rational
lagrangePoly xs n = sum $ do
  ((i,v),ys) <- pick xs
  pure $ fromIntegral v * product (fmap (\(i1,_) -> fInt (n - i1) % fInt (i - i1)) ys)

result :: [Rational]
result = fmap (lagrangePoly [(1,2),(3,4),(9,12)]) [1..10]


