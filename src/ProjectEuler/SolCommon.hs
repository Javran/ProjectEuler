module ProjectEuler.SolCommon
  ( factorials
  , factorial
  , intToDigits
  , digitsToInt
  , intToDigitsRev
  , pick
  , numReverseInBase
  ) where

import Data.List

{-
  This module contains some commonly used functions
  that will eventually be moved into petbox.
  The idea here is to use this module as an intermediate place
  so that we don't need to update petbox that often.

  TODO: utilize hspec to at least get some example covered.
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

-- | non-deterministically picking an element from the given list,
--   separating the selected element and all other remaining elements
--   the list order is preserved
--   e.g. pick [1,2,3] == [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"
{-# INLINABLE pick #-}

-- convert to reversed list of digits and put digits together,
-- this allows digit-wise reversal of numbers
-- and gives a compact representation (i.e. the number itself) to work with
-- rather than comparing on list of stuff.
numReverseInBase :: Integral i => Int -> i -> i
numReverseInBase base = foldl (\a b -> a*base'+b) 0 . unfoldr f
  where
    base' = fromIntegral base
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` base' in Just (r, q)
{-# INLINABLE numReverseInBase #-}
{-# SPECIALIZE numReverseInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE numReverseInBase :: Int -> Integer -> Integer #-}
