{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module ProjectEuler.Problem92
  ( problem
  ) where

import Data.Array
import Data.Array.ST
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 92 Solved result

data Result
  = Unknown
  | StuckIn1
  | StuckIn89
  deriving (Eq)

intToDigitsRev :: Int -> [Int]
intToDigitsRev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q,r) = n `quotRem` 10 in Just (r, q)

squareDigit :: Int -> Int
squareDigit n = sum $ sq <$> intToDigitsRev n

-- since we only search in range 1..9,999,999
-- we can preprocess numbers in 1 .. 7 * 9^2
-- and `sqyareDigit` is sure to bring any number into this range

arriveTable :: Array Int Result
arriveTable = runSTArray $ do
    let upBound = 7 * sq 9
    mary <- newArray (1,upBound) Unknown
    writeArray mary 1  StuckIn1
    writeArray mary 89 StuckIn89
    let touch n = do
          res <- readArray mary n
          if res == Unknown
            then do
              res' <- touch (squareDigit n)
              writeArray mary n res'
              pure res'
            else pure res
    mapM_ touch [1..upBound]
    pure mary

result :: Int
result = length
       . filter ((== StuckIn89) . (arriveTable !) . squareDigit)
       $ [1..9999999]

