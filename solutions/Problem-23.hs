import Data.List
import qualified Data.List.Ordered as LO

-- TODO: need some work, skipping for now

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
divisors x = init $ sort $ LO.union leftHalf rightHalf
    where
        leftHalf = [ y | y <- [1..x], y * y <= x, x `mod` y == 0]
        rightHalf = reverse [x `div` y | y <- leftHalf]

isAbundant :: Int -> Bool
isAbundant n = n < sum (divisors n)

-- remove duplicate elements from a sorted list
removeDup :: [Int] -> [Int]
removeDup (x:xs) = reverse $ foldl (\acc i -> if head acc == i then acc else i:acc) [x] xs
removeDup [] = []

maxAbun = 28123

main = do
    let possibleAbuns = takeWhile (<= maxAbun) $ filter isAbundant [1..]
    let reachables = sort $ do
          x <- possibleAbuns
          y <- takeWhile (\u -> u + x <= maxAbun) possibleAbuns
          return (x+y)
    print $ sum $ LO.minus [1..maxAbun] $ LO.nub reachables
