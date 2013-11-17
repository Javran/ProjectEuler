import Data.Numbers.Primes
import Data.List

findConsecutive :: (Integral a, Show a) => [a] -> Int -> [a] -> [a]
findConsecutive conList len restList
    | len == length conList = reverse conList
    | otherwise = if length (nub $ primeFactors hd) >= 4
                    then findConsecutive (hd:conList) len tl
                    else findConsecutive [] len tl
                    where (hd:tl) = restList

main = print $ findConsecutive [] 4 [1..]
