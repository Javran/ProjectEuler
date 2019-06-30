{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem19
  ( problem
  ) where

import Data.Time.Calendar
import Data.Monoid
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 19 Solved result

-- given year and month, return a list of days
dayList :: Int -> Int -> [Int]
dayList year month = [1..gregorianMonthLength (fromIntegral year) month]

-- a list of dates: [(m,d)]
monthList :: Int -> [(Int,Int)]
monthList year = concatMap (\m -> (m,) <$> dayList year m) [1..12]

dateList :: [(Int, Int, Int)]
dateList = concatMap (\y -> map (\(m,d) -> (y,m,d)) (monthList y)) [1900..]

dayOfWeekCycle :: [Int]
dayOfWeekCycle = cycle [1..7]

-- TODO: could operate on Data.Time.Calendar.Day?

result :: Int
result =
    let dateNotBegin (_, (y,_,_)) = y < 1901
        dateNotEnd   (_, (y,_,_)) = y < 2001
        searchSpace =
          takeWhile dateNotEnd
          . dropWhile dateNotBegin
          $ zip dayOfWeekCycle dateList
        valid (7, (_,_,1)) = 1
        valid _ = 0
    in getSum $ foldMap valid searchSpace

