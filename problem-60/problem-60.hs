{-# LANGUAGE TupleSections #-}
import Data.Numbers.Primes
import Data.Function
import Data.List
import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Applicative
import Control.Arrow
import Data.Tuple

groupLen :: Int
groupLen = 5

-- | primes under a constant number
-- by many times of execution, we find setting this constant
-- to 10000 helps us find the first solution
limitedPrimes :: [Int]
limitedPrimes = takeWhile (<= 10000) primes

-- | concatenate two numbers as if concatenating two strings
concatNum :: Int -> Int -> Int
-- this implementation is more efficient,
-- as this function is heavily used, we want it to be fast
concatNum a b = b + a * mult
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
primePairs = concat (zipWith (\a b -> map (a,) (tail b)) limitedPrimes (tails limitedPrimes))

validPrimePairs :: [(Int,Int)]
validPrimePairs = filter hasConcatProperty primePairs

-- | concat property: if we use <+> as num "concat",
-- then concat property says that A<+>B is a prime, and B<+>A is also a prime.
hasConcatProperty :: (Int,Int) -> Bool
hasConcatProperty = ((&&) `on` isPrime) <$> cn <*> (cn . swap)
    where
        cn = uncurry concatNum

solve :: IM.IntMap IS.IntSet -> [Int] -> [Int] -> [[Int]]
solve pairTable candidates chosen
    | length chosen == groupLen = [chosen]
    | otherwise = do
        -- pick up one candidate
        next <- candidates
        let nextNeighbors = IM.findWithDefault IS.empty next pairTable
        -- verify that it can "connect" with all chosen candidates
        guard $ all (`IS.member` nextNeighbors) chosen
        let newCandidates = filter (`IS.member` nextNeighbors) candidates
        -- further filter out members according to the new "connects with" relation
        solve pairTable newCandidates (next:chosen)

main :: IO ()
main = do
    let occur = IM.fromListWith
                  (IS.union)
                  (map
                   (second IS.singleton)
                   (validPrimePairs ++ map swap validPrimePairs))
        allCandidates = sort $ nub $ concatMap (\(x,y) -> [x,y]) validPrimePairs
        firstSolution = reverse (head (solve occur allCandidates []))
    print firstSolution
    print (sum firstSolution)

{-

compile with O2 flag.

real    1.45s
user    1.44s
sys     0.00s

-}
