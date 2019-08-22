{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem119
  ( problem
  ) where

import Data.Function
import Data.Ord
import qualified Data.List.Ordered as LOrdered

import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 119 Solved result

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
    {-
      this is important to use merge rather than union,
      as we do have cases where some numbers are powers of multiple numbers.
      also we don't really need to resolve base on `fst` part - the problem
      itself doesn't really care which base you pick as long as
      it meets the requirement.
     -}
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
result =
  {-
    the number 100 is just a random choice,
    as we are merging all the streams,
    we cannot take an infinite list to generate their powers
    because otherwise we will never be able to find the first element.

    Note by using "nubBy" below we generate a sequence of unique numbers,
    for this problem it doesn't matter as it so happens that no duplicate element
    has occurred - but this is a good precaution to have anyway.
   -}
  snd $ LOrdered.nubBy ((<) `on` snd) (genSeq 100) !! 29
