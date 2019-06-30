module ProjectEuler.Problem12
  ( problem
  ) where

import Control.Arrow
import ProjectEuler.Types
import Math.NumberTheory.Primes
import Petbox hiding (primes) -- TODO: prefer arithmoi.

problem :: Problem
problem = pureProblem 12 Solved result

factors :: Int -> [(Int,Int)]
factors x = first fromIntegral <$> factorise' (fromIntegral x)

divisorCount :: Int -> Int
divisorCount n = product $ map ((+1) . snd) $ factors n

result :: Int
result = firstSuchThat ((> 500) . divisorCount) $ scanl (+) 1 [2..]

