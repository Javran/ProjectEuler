module ProjectEuler.Problem130
  ( problem
  ) where

import Math.NumberTheory.Primes

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types
import ProjectEuler.Problem129 (genInput, computeA)

problem :: Problem
problem = pureProblem 130 Solved result

{- This is easy to solve once Problem129 is solved. -}

inputs :: [Int]
inputs =
  LOrdered.minus'
    (genInput 90) -- since we know 91 is the first one, might as well start with 91.
    (unPrime <$> primes)

result :: Int
result =
  sum . take 25 $ filter (\x -> let r = computeA x in (x - 1) `rem` r == 0) inputs

