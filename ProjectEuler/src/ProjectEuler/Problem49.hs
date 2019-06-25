module ProjectEuler.Problem49
  ( problem
  ) where

import Control.Monad
import Data.Int
import Data.List
import Math.NumberTheory.Primes

import qualified Data.IntMap.Strict as IM
import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 49 Solved result

-- step 1: get primes
-- step 2: group by permutation closure
-- step 3: find arithmetic seq

intToDigits :: Int -> [Int]
intToDigits x = ($ []) . foldr (.) id $ unfoldr f x
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just ((++[r]), q)

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\a b -> a*10+b) 0

limitedPrimes :: [Int]
limitedPrimes = takeWhile (< 10000) $ dropWhile (< 1000) primes

-- assume arithmetic sequence should at least have 2 elements
arithSeq :: [Int] -> [[Int]]
arithSeq arr = do
    let n = length arr
    i <- [0..n-1]
    j <- [i+1..n-1]
    pure $ growPair (arr !! i) (arr !! j)
  where
    -- given that a1 and a2 are in the list, grow to form
    --   the longest seq using elements from arr
    growPair :: Int -> Int -> [Int]
    growPair a1 a2 =
      if a3 `elem` arr
        then a1 : growPair a2 a3
        else [a1,a2]
      where
        a3 = a2 + (a2 - a1)

-- only interested in long seq
longArithSeq :: [Int] -> [[Int]]
longArithSeq = foldMap f . arithSeq
  where
    -- at least 3 elements
    f xs@(_:_:_:_rest) = [xs]
    f _ = []

result :: Int64
result = read $ foldMap show ans
  where
    -- group together prime number that are permutations of each other.
    -- elements of this list is sorted (by using LOrdered.union).
    primeGroups :: [[Int]]
    primeGroups = IM.elems (IM.fromListWith LOrdered.union (toPair <$> limitedPrimes))
      where
        toPair x = (digitsToInt . sort . intToDigits $ x, [x])
    [[ans]] = do
      -- for each group, we want every elements in it converted back to a num
      --   we only need closures with size >= 3
      pg@(_:_:_:_:_rest) <- primeGroups
      -- xs is not null
      xs@(_:_) <- [longArithSeq pg]
      -- since the list is sorted, we just want to check the first
      -- element of the group to exclude the known one.
      guard $ (/= 1487) . head . head $ xs
      pure xs
