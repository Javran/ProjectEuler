{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem37
  ( problem
  ) where

import Data.List
import Math.NumberTheory.Primes.Testing (isPrime)
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 37 Solved result

{-

  TODO: might worth making following functions common:

  - factorial, fib
  - numReverseInBase

 -}
isTruncatable :: Int -> Bool
isTruncatable x =
    all (isPrime . fromIntegral @Int) $ possibleNumLeft <> possibleNumRight
  where
    digitsX = intToDigits x
    possibleNumLeft =
      -- tails [x,y,z] = [[x,y,z],[y,z],[z],[]],
      -- therefore we want to get rid of first and last element of it.
      digitsToInt <$> init (tail (tails digitsX))
    possibleNumRight =
      -- inits [x,y,z] = [[],[x],[x,y],[x,y,z]],
      -- same story, but just need to get rid of the first element.
      digitsToInt <$> tail (inits digitsX)

result :: Int
result = sum $ take 11 $ filter isTruncatable searchSpace
  where
    searchSpace = dropWhile (<=7) primes
