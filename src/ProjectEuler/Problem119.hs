{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem119
  ( problem
  ) where

import Data.Ord
import qualified Data.List.Ordered as LOrdered

import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 119 Unsolved result

{-
  This could be very interesting to do in a non-strict language:
  obviously searching all integers to find this property is not practical,
  so instead, we can explore numbers raised to some powers and check
  whether they meets the criteria.
  We can first define list of numbers raised to square, cube, etc.
  merge them and filter through it.

 -}
powers :: Integer -> [Integer]
powers x = iterate (*x) x

genPowers :: Integer -> [(Integer, Integer)]
genPowers upperBound =
  foldl1
    (LOrdered.mergeBy (comparing snd))
    ((\x -> (x,) <$> powers x) <$> [2..upperBound])

genSeq :: Integer -> [(Integer, Integer)]
genSeq upperBound =
    filter isValid
    . dropWhile ((< 10) . snd)
    $ genPowers upperBound
  where
    isValid :: (Integer, Integer) -> Bool
    isValid (base, v) = base == fromIntegral (sum (intToDigitsRev v))

result :: Integer
result = snd $ genSeq 100 !! 29
