{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem101
  ( problem
  ) where

import Data.Ratio
import Data.Monoid
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 101 Solved result

-- 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
u :: Int -> Integer
u n =
  sum
  $ zipWith (*)
      (cycle [1,-1])
      (take 11 $ iterate (* n') 1)
  where
    n' :: Integer
    n' = fInt n

uValues :: [Integer]
uValues = u <$> [1..]

{-
  Lagrange polynomial: https://en.wikipedia.org/wiki/Lagrange_polynomial
 -}
lagrangePoly :: [(Int,Integer)] -> Int -> Integer
lagrangePoly xs n = round @(Ratio Integer) . sum $ do
  ((i,v),ys) <- pick xs
  pure $
    fromIntegral v *
    product ((\(i1,_) -> fInt (n-i1) % fInt (i-i1)) <$> ys)

findFirstIncorrect :: Int -> Sum Integer
findFirstIncorrect l =
    Sum
    . fst . head
    . filter (\(x,y) -> x /= y)
    $ zip (f <$> [l+1,l+2..]) ys
  where
    (xs,ys) = splitAt l uValues
    f = lagrangePoly (zip [1..] xs)

result :: Integer
result = sum $ foldMap findFirstIncorrect [1..10]
