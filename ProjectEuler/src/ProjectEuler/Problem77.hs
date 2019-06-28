module ProjectEuler.Problem77
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 77 Solved result

numParts :: Int -> Int -> [ [Int] ]
numParts m n
    | n > m = []
    | n == m = [ [n] | isPrime (fromIntegral m)]
    | not (isPrime (fromIntegral n)) = []
    | otherwise = do
        n' <- takeWhile (<= n) primes
        xs <- numParts (m-n) n'
        pure $ n:xs

countParts :: Int -> [] [Int]
countParts x = [1..x] >>= numParts x

result :: Int
result = firstSuchThat ((> 5000) . length . countParts) [1..]

