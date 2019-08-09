{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem60
  ( problem
  ) where

import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Tuple
import Math.NumberTheory.Primes.Testing (isPrime)
import Petbox

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 60 Solved result

-- | concatenate two numbers as if concatenating two strings
concatNum :: Int -> Int -> Integer
-- this implementation is more efficient,
-- as this function is heavily used, we want it to be fast
concatNum a b = fromIntegral $ b + a * mult
  where
    mult :: Int
    mult = 10 ^ (1 + log10 (fromIntegral b))
    log10 :: Double -> Int
    log10 x = floor (logBase 10 x)

-- a more readable version of the same function above
-- concatNum :: Int -> Int -> Int
-- concatNum = read .: ((++) `on` show)

-- | all pairs of prime numbers under a constant number (1000 for now)
--  for any (i,j) from this list, it's guaranteed that i < j
primePairs :: [(Int,Int)]
primePairs =
    concat $
      zipWith
        (\a b -> (a,) <$> tail b)
        limitedPrimes
        (tails limitedPrimes)
  where
    limitedPrimes = takeWhile (<= 10000) primes


-- | concat property: if we use <+> as num "concat",
-- then concat property says that A<+>B is a prime, and B<+>A is also a prime.
hasConcatProperty :: (Int,Int) -> Bool
hasConcatProperty = ((&&) `on` isPrime) <$> cn <*> (cn . swap)
  where
    cn = uncurry concatNum

solve :: IM.IntMap IS.IntSet -> IS.IntSet -> [Int] -> [[Int]]
solve pairTable candidates chosen
    | length chosen == 5 = [chosen]
    | otherwise = do
        -- pick up one candidate
        next <- IS.toAscList candidates
        let nextNeighbors = IM.findWithDefault IS.empty next pairTable
            newCandidates = IS.filter (`IS.member` nextNeighbors) candidates
        -- verify that it can "connect" with all chosen candidates
        guard $ all (`IS.member` nextNeighbors) chosen
        -- further filter out members according to the new "connects with" relation
        solve pairTable newCandidates (next:chosen)

result :: Int
result = sum firstSolution
  where
    validPrimePairs :: [(Int,Int)]
    validPrimePairs = filter hasConcatProperty primePairs
    allCandidates = IS.fromList $ concatMap (\(x,y) -> [x,y]) validPrimePairs
    occur =
      IM.fromListWith
        IS.union
          $ second IS.singleton <$> (validPrimePairs <> (swap <$> validPrimePairs))
    (firstSolution:_) = solve occur allCandidates []
