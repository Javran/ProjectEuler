import Data.Numbers.Primes
import Data.List

-- observe that 0,2,4,6,8 should not appear in a circular prime
-- "2" is a special case
searchSpace :: [Int]
searchSpace = 2: filter (not.hasEven) (takeWhile (<1000000) primes)
    where
        hasEven x = any (`elem` show x) "02468"

circularNums :: Int -> [Int]
circularNums n = take len $ iterate doCirNum n
    where
        len = length $ show n
        doCirNum num = read (tl ++ [hd])
            where (hd:tl) = show num

findCirculars curSpace foundPrimes
    | null curSpaceValid = foundPrimes
    | all (`elem` curSpaceValid) cirH = findCirculars tl (nub (foundPrimes ++ cirH))
    | otherwise = findCirculars (tl \\ cirH) foundPrimes
    where
        curSpaceValid = curSpace \\ foundPrimes
        (hd:tl) = curSpaceValid
        cirH = circularNums hd

main =
    print $ length $ findCirculars searchSpace []
