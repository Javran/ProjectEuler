import Control.Monad
import Data.List

-- from problem 24
factorial n = product [1..n]

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

threeParts :: [(Int, Int, Int)]
threeParts = do
    a <- [1..9]
    b <- [a..9]
    let c = 9-a-b
    guard $ a+b-1 <= c && c <= a+b
    return (a,b,c)

threePartition :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
threePartition xs (a,b,c) = (digitToNum aPart, digitToNum bPart, digitToNum cPart)
    where (aPart,xs1) = splitAt a xs
          (bPart,xs2) = splitAt b xs1
          (cPart,  _) = splitAt c xs2

digitToNum :: [Int] -> Int
digitToNum = foldl (\acc i -> acc*10+i) 0

main = do
    let allPerms = map (`pidToPermutation` [1..9]) [0..factorial 9 -1]
    let searchSpace = concatMap (\perm -> map (threePartition perm) threeParts) allPerms
    let valid (x,y,z) = x*y == z
    let answers = filter valid searchSpace
    print $ sum $ nub $ map (\(_,_,z) -> z) answers

{-
compile with: ghc -O problem-32.hs

real    0m0.595s
user    0m0.587s
sys     0m0.007s
-}
