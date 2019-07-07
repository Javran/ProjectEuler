module ProjectEuler.SolCommon
  ( factorials
  , factorial
  , intToDigits
  , digitsToInt
  , intToDigitsRev
  ) where

import Data.List

{-
  This module contains some commonly used functions
  that will eventually be moved into petbox.
  The idea here is to use this module as an intermediate place
  so that we don't need to update petbox that often.
 -}

factorials :: [Int]
factorials = scanl (*) 1 [1..]
{-# INLINABLE factorials #-}

factorial :: Int -> Int
factorial = (factorials !!)
{-# INLINABLE factorial #-}

intToDigits :: Integral i => i -> [Int]
intToDigits x = ($ []) . foldr (.) id $ unfoldr f x
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just ((++[fromIntegral r]), q)
{-# INLINABLE intToDigits #-}
{-# SPECIALISE intToDigits :: Int -> [Int] #-}

digitsToInt :: Integral i => [Int] -> i
digitsToInt = foldl (\a b -> fromIntegral a*10+ fromIntegral b) 0
{-# INLINABLE digitsToInt #-}
{-# SPECIALIZE digitsToInt :: [Int] -> Int #-}

intToDigitsRev :: Integral i => i -> [Int]
intToDigitsRev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just (fromIntegral r, q)
{-# INLINABLE intToDigitsRev #-}
{-# SPECIALIZE intToDigitsRev :: Int -> [Int] #-}
{-# SPECIALIZE intToDigitsRev :: Integer -> [Int] #-}
