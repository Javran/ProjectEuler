module ProjectEuler.Everything
( factorial
, permuteTo
, removeElement
, allPermutation
, digitToNum
, arithmeticSequenceLength
) where

factorial :: Int -> Int
factorial x = product [1..x]

-- get the (n+1)-th permutation of a list
--   all elements in the list should be distinct
--   and the valid range of n is :
--     0 <= n <= factorial (length arr)
permuteTo :: (Eq a) => Int -> [a] -> [a]
permuteTo 0 arr = arr
permuteTo pid arr = h:(permuteTo subPid t)
    where
        n = length arr
        subCycle = factorial $ n - 1
        headInd = pid `div` subCycle
        (h,t) = removeElement headInd arr
        subPid = pid `mod` subCycle

-- gives you all permutation of xs
allPermutation :: (Eq a) => [a] -> [[a]]
allPermutation xs = map (flip permuteTo xs) $ take (factorial (length xs)) [0..]

-- fetch and remove the n-th element from a list
removeElement :: (Eq a) => Int -> [a] -> (a,[a])
removeElement 0 (x:xs) = (x,xs)
removeElement n (x:xs) = (x1,x:xs1)
    where
        (x1,xs1) = removeElement (n-1) xs

digitToNum :: [Int] -> Int
digitToNum = foldl (\acc i -> acc*10 + i) 0

-- find the longest arithmetic sequence from begining
arithmeticSequenceLength :: (Num a, Eq a) => [a] -> Int
arithmeticSequenceLength xs =
    case xs of
        []     -> 0
        (a:[]) -> 1
        _      -> 1 + (length $ takeWhile (\x -> x == (head diff)) diff)
            where
                diff = zipWith (-) xs (tail xs)
