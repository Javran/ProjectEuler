module ProjectEuler.Problem145
  ( problem
  ) where

import Petbox
import Data.Monoid

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 145 Solved result

{-
  Idea:

  A brute force gets the answer.

  Afterthoughts: turns out there is no 9-digit solutions,
  which allows us to simply search 1~10^8-1 to get the right answer.

  TODO: could do better, base on number of digits n mod 4.

 -}

isReversible :: Int -> Bool
isReversible x = x `rem` 10 /= 0 && all odd xs
  where
    xs = intToDigitsRev (x + numReverseInBase 10 x)

result :: Int
result =
  getSum
  . foldMap (\x -> if isReversible x then 1 else 0) $
    [1..10^!8-1]

