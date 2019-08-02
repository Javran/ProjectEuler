module ProjectEuler.Problem65
  ( problem
  ) where

import Data.Ratio
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 65 Solved result

eExpand :: [Integer]
eExpand = 2 : concatMap (\x -> [1,2*x,1]) [1..]

approx :: [Integer] -> Rational
approx [] = error "invalid input"
approx [x] = x%1
approx (x:xs) = (x%1) + (1%1) / approx xs

result :: Int
result =
  sum
  . intToDigitsRev
  . numerator
  . approx
  $ take 100 eExpand
