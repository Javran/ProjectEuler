import Data.Numbers.Primes
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

-- step 1: get primes
-- step 2: group by permutation closure
-- step 3: find arithmetic seq

limitedPrimes = takeWhile (<10000) $ dropWhile (<1000) primes

numToDigitList num = map (\ch -> ord ch - ord '0') $ show num

digitListToNum = foldl (\acc i -> acc * 10 + i) 0

-- remove first occurrence of x from ls
removeFirst x ls = before ++ after
    where
        before = takeWhile (/=x) ls
        after = tail $ dropWhile (/=x) ls

newtype Permu = Permu [Int]
    deriving Show

instance Eq Permu where
    (Permu x) == (Permu y) = sort x == sort y

instance Ord Permu where
    (Permu x) `compare` (Permu y) = sort x `compare` sort y

updateMap m p = newMap
    where
        result = M.lookup p m
        (Permu xs) = p
        newMap = if isNothing result
            then M.insert p [xs] m
            else M.update (Just . (xs:)) p m

-- given that a1 and a2 are in the list, grow to form
--   the longest seq using elements from arr
growPair arr a1 a2 =
    if a3 `elem` arr
        then a1: growPair arr a2 a3
        else [a1,a2]
    where a3 = a2 + (a2 - a1)

-- assume arithmetic sequence should at least have 2 elements
arithSeq arr = do
    let n = length arr
    i <- [  0..n-1]
    j <- [i+1..n-1]
    return $ growPair arr (arr !! i) (arr !! j)

-- only interested in long seq
longArithSeq arr = filter ((>2).length) $ arithSeq arr

main = do
    let permuPrimeList = map (Permu . numToDigitList) limitedPrimes
    let m = map snd $ M.toList $ foldl updateMap M.empty permuPrimeList
    -- for each group, we want every elements in it converted back to a num
    --   we only need closures with size >= 3
    let permClosures = map sort $ filter ((>= 3).length) $ map (liftM digitListToNum) m

    mapM_ print $ filter (not.null) $ map longArithSeq permClosures
