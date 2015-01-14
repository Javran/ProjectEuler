import Data.List

{-
compile with `-O`
time:
real    0m25.942s
user    0m25.440s
sys     0m0.520s
-}

-- return a list of divisors of x
divisors :: Int -> [Int]
-- guaranteed that the last one should be x, remove it safely
divisors x = init $ sort $ union leftHalf rightHalf
    where
        leftHalf = [ y | y <- [1..x], y * y <= x, x `mod` y == 0]
        rightHalf = [x `div` y | y <- leftHalf]

isAbundant :: Int -> Bool
isAbundant n = n < sum (divisors n)

-- remove duplicate elements from a sorted list
removeDup :: [Int] -> [Int]
removeDup (x:xs) = reverse $ foldl (\acc i -> if head acc == i then acc else i:acc) [x] xs
removeDup [] = []

-- x \\ y, assuming x and y are all sorted
diffSorted :: [Int] -> [Int] -> [Int]
diffSorted [] _ = []
diffSorted xs [] = xs
diffSorted (x:xs) (y:ys)
    | x > y     = diffSorted (x:xs) ys
    | x < y     = x:diffSorted xs (y:ys)
    | otherwise = diffSorted xs (y:ys) 

maxAbun = 28123

main = do
    let possibleAbuns = takeWhile (<= maxAbun) $ filter isAbundant [1..]
    let reachables = sort $ do
        x <- possibleAbuns
        y <- takeWhile (\u -> u + x <= maxAbun) possibleAbuns
        return (x+y)
    print $ sum $ diffSorted [1..maxAbun] $ removeDup reachables
