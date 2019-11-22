module ProjectEuler.Problem12
  ( problem
  ) where

import Control.Arrow
import ProjectEuler.Types
import Math.NumberTheory.Primes.Factorisation
import Petbox

problem :: Problem
problem = pureProblem 12 Solved result

factors :: Int -> [(Int,Int)]
factors x = (fromIntegral *** fromIntegral) <$> factorise' (fromIntegral x)

divisorCount :: Int -> Int
divisorCount n = product $ map ((+1) . snd) $ factors n

result :: Int
result = firstSuchThat ((> 500) . divisorCount) $ scanl (+) 1 [2..]

