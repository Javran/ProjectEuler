module ProjectEuler.Problem23
  ( problem
  ) where

import Math.NumberTheory.ArithmeticFunctions
import Data.Monoid
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 23 Solved result

-- see: http://mathschallenge.net/library/number/sum_of_divisors
divisorSum :: Int -> Int
divisorSum = sum . divisorsList

isAbundant :: Int -> Bool
isAbundant n = n < divisorSum n - n

result :: Int
result =
    getSum $ foldMap (\x -> if IS.member x reachables then 0 else Sum x) range
  where
    range = [1..maxAbun]
    possibleAbuns = filter isAbundant range
    maxAbun = 28123
    reachables =
      IS.fromList [ x+y
                  | x <- possibleAbuns
                  , y <- takeWhile (<= maxAbun - x) possibleAbuns
                  ]
