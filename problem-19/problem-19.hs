divisibleBy :: Int -> Int -> Bool
x `divisibleBy` y = x `mod` y == 0

isLeap :: Int -> Bool
isLeap x =
       (not (x `divisibleBy` 100) && (x `divisibleBy`   4))
    || (    (x `divisibleBy` 100) && (x `divisibleBy` 400))

-- given year and month, return a list of days
dayList :: Int -> Int -> [Int]
dayList year month = [1..lastDay month]
    where
        lastDay m
            | m `elem` [1,3,5,7,8,10,12]  = 31
            | m == 2                      = 28 + if isLeap year then 1 else 0
            | otherwise                   = 30

-- a list of dates: [(m,d)]
monthList :: Int -> [(Int,Int)]
monthList year = concatMap (\m -> map (\d -> (m,d)) $ dayList year m) [1..12]

dateList :: [(Int, Int, Int)]
dateList = concatMap (\y -> map (\ (m,d) -> (y,m,d))  (monthList y) ) [1900..]

dayOfWeekCycle :: [Int]
dayOfWeekCycle = cycle [1..7]

main = do
    let dataNotBegin (dow, (y,m,d)) = y < 1901
    let dataNotEnd   (dow, (y,m,d)) = y < 2001

    let searchSpace = takeWhile dataNotEnd $ dropWhile dataNotBegin $ zip dayOfWeekCycle dateList

    let valid (dow, (y,m,d)) = dow == 7 && d == 1

    print $ length $ filter valid searchSpace
