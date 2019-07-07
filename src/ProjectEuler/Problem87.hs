module ProjectEuler.Problem87
  ( problem
  ) where

import Petbox
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 87 Solved result

primesP2, primesP3, primesP4 :: [Int]
primesP2 = (^! 2) <$> primes
primesP3 = (^! 3) <$> primes
primesP4 = (^! 4) <$> primes

searchSpace :: [Int]
searchSpace = do
    pFt <- takeWhile (<= limit) primesP4
    pCb <- takeWhile (<= limit - pFt) primesP3
    let tmp = pFt + pCb
    pSq <- takeWhile (<= limit - tmp) primesP2
    pure (pSq + tmp)
  where
    limit = 50000000

result :: Int
result = IS.size $ IS.fromList searchSpace

