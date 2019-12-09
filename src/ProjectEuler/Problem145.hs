module ProjectEuler.Problem145
  ( problem
  ) where

import Petbox
import Data.Monoid

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 145 Unsolved result

isReversible :: Int -> Bool
isReversible x = x `rem` 10 /= 0 && all odd xs
  where
    xs = intToDigitsRev (x + numReverseInBase 10 x)

result :: Int
result =
  getSum $ foldMap (\x -> if isReversible x then 1 else 0) [1..1000000000-1]

