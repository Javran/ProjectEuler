module ProjectEuler.Problem35
  ( problem
  , intToDigits
  , circularNums
  ) where

import Math.NumberTheory.Primes
import Data.List
import Data.Bits
import qualified Data.List.Match as LMatch

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 35 Solved result

-- observe that 0,2,4,6,8 should not appear in a circular prime
-- "2" is a special case
searchSpace :: [Int]
searchSpace = 2 : filter hasNoEven (takeWhile (<1000000) primes)
  where
    hasNoEven :: Int -> Bool
    hasNoEven = all (`testBit` 0) . intToDigits

circularNums :: Int -> [Int]
circularNums n = takeL $ digitsToInt . takeL <$> tails (cycle ds)
  where
    takeL = LMatch.take ds
    ds = intToDigits n

findCirculars :: [Int] -> [Int] -> [Int]
findCirculars curSpace foundPrimes
  | null curSpaceValid = foundPrimes
  | all (`elem` curSpaceValid) cirH = findCirculars tl (nub (foundPrimes ++ cirH))
  | otherwise = findCirculars (tl \\ cirH) foundPrimes
  where
    curSpaceValid = curSpace \\ foundPrimes
    (hd:tl) = curSpaceValid
    cirH = circularNums hd

intToDigits :: Int -> [Int]
intToDigits x = ($ []) . foldr (.) id $ unfoldr f x
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just ((++[r]), q)

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\a b -> a*10+b) 0

result :: Int
result =
  length $ findCirculars searchSpace []

