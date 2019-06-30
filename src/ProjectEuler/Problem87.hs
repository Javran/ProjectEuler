module ProjectEuler.Problem87
  ( problem
  ) where


import Petbox
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 87 Solved result

searchSpace :: [Int]
searchSpace = do
    pFt <-
      takeWhile leLimit
      $ (^! 4) <$> primes
    pCb <-
      takeWhile (leLimit . (+ pFt))
      $ (^! 3) <$> primes
    pSq <-
      takeWhile (leLimit . (+ (pFt+pCb)))
      $ (^! 2) <$> primes
    pure (pSq + pCb + pFt)
  where
    leLimit = (<= 50000000)

result :: Int
result = IS.size $ IS.fromList searchSpace

