module ProjectEuler.Problem23
  ( problem
  ) where

import Math.NumberTheory.Primes.Factorisation (factorise)
import qualified Math.NumberTheory.Primes as Primes
import Data.Monoid
import Control.Arrow
import qualified Data.IntSet as IS
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 23 Solved result

maxAbun :: Int
maxAbun = 28123

primes' :: [Int]
primes' = takeWhile (<= maxAbun) $ Primes.unPrime <$> Primes.primes

factorise1 :: Int -> [(Int, Int)]
factorise1 = auxFactor primes'
  where
    auxFactor [] 1 = []
    auxFactor [] _ = error "unreachable"
    auxFactor (p:ps) n
      | n `rem` p == 0 =
          let pPows = takeWhile ((<=n) . snd) $ iterate (succ *** (*p)) (1,p)
              (count, pz) = last $ filter (\(_,p') -> n `rem` p' == 0) pPows
          in (p,count) : auxFactor ps (n `quot` pz)
      | otherwise = auxFactor ps n

-- see: http://mathschallenge.net/library/number/sum_of_divisors
divisorSum :: Int -> Int
divisorSum n = product $ dSumPrimePow <$> fs
  where
    dSumPrimePow (p,a) = (p'^(a+1) - 1) `quot` (p'-1)
      where
        p' = fromIntegral p
    fs = factorise (fromIntegral n)

isAbundant :: Int -> Bool
isAbundant n = n < divisorSum n - n

result :: Int
result =
    getSum $ foldMap (\x -> if IS.member x reachables then 0 else Sum x) [1..maxAbun]
  where
    possibleAbuns = filter isAbundant [1..maxAbun]

    reachables =
      IS.fromList $ do
        x <- possibleAbuns
        y <- takeWhile (<= maxAbun - x) possibleAbuns
        pure (x+y)
