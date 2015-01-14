import Data.Char

-- use permutation algorithm from problem 24
factorial n = product [1..n]

-- define `permutation id` x to be the identity of the (x+1)-th permutaion
-- e.g. 0,1,2, pid 0 => 0,1,2, pid 1 => 0,2,1, ... etc.
-- the problem is to find the (999999+1)-th permutation for [0..9], pid = 999999
pidToPermutation :: Int -> [Int] -> [Int]
pidToPermutation 0 arr = arr
pidToPermutation pid arr = h:pidToPermutation subPid t
    where
        n = length arr
        subCycle = factorial $ n - 1
        headInd = pid `div` subCycle
        (h,t) = breakList headInd arr
        subPid = pid `mod` subCycle

-- get the x-indexed element, return the list after removing that element
breakList :: (Eq a) => Int -> [a] -> (a,[a])
breakList 0 (x:xs) = (x,xs)
breakList n (x:xs) = (x1,x:xs1)
    where
        (x1,xs1) = breakList (n-1) xs

toNum = foldl (\acc i-> acc*10 + i) 0 

valid xs = all (uncurry (\num prime -> num `mod` prime == 0)) tests
    where
        numList = [ indToNum (x,x+1,x+2) | x <- [1..7]]
        indToNum (a,b,c) = sum $ zipWith (*) [xs !! a, xs !! b, xs !! c] [100,10,1]
        primeList = [2,3,5,7,11,13,17]
        tests = zip numList primeList

main =
    print $ sum $ map toNum $ filter valid $ map (`pidToPermutation` [0..9]) [0..factorial 10 -1]

{-
compile with: ghc -O
time:
real    0m3.516s
user    0m3.477s
sys     0m0.040s
-}
