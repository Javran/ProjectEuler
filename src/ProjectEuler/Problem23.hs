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
divisorSum = sum . IS.toList . divisorsSmall

isAbundant :: Int -> Bool
isAbundant n = n < divisorSum n - n

result :: Int
result =
    getSum $ foldMap (\x -> if IS.member x reachables then 0 else Sum x) [1..maxAbun]
  where
    possibleAbuns = filter isAbundant [1..maxAbun]
    maxAbun = 28123
    reachables =
      IS.fromList $ do
        x <- possibleAbuns
        y <- takeWhile (<= maxAbun - x) possibleAbuns
        pure (x+y)
