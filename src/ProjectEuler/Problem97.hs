{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem97
  ( problem
  ) where

import Data.Int
import Data.Function

import Math.NumberTheory.Powers.Squares

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 97 Solved result

base :: Integral i => i
base = 10000000000

lim :: Int64
lim = fromInteger $ integerSquareRoot' (fromIntegral @Int64 @Integer maxBound)

{-
  2^64 has 20 digits in decimal, so if we are multiplying
  two numbers that both have 10 digits, overflow could happen,
  to deal with this, we use Integer to store the intermediate result
  so that overall we are only passing Int64 between functions.
 -}
mul :: Int64 -> Int64 -> Int64
mul x y
  | x <= lim && y <= lim =
    {-
      If both x and y are within representation range of Int64,
      there's no risk of overflow, this avoids the expensive
      mulplication for Integer type.
     -}
    (x * y) `mod` base
  | otherwise =
    fromInteger $ ((*) `on` fromIntegral) x y `mod` base

{-
  the following function is ported from
  Math.NumberTheory.Powers.Modular.powMod with restricted type
  and a fixed base.
  In addition, (*) is replaced by mul to allow only passing Int64
  between functions while avoiding overflow.
 -}
powMod :: Int64 -> Int -> Int64
powMod x y = f (x `rem` base) y 1 `mod` base
  where
    f _ 0 acc = acc
    f b e acc =
      f (mul b b `rem` base) (e `quot` 2) $
        if odd e
          then mul b acc `rem` base
          else acc

result :: Int64
result = (28433 * powMod 2 7830457 + 1) `rem` base
