{-# LANGUAGE RankNTypes, FlexibleContexts #-}

import Petbox
import Data.Array
import Data.Array.ST

data Result = Unknown
            | StuckIn1
            | StuckIn89
  deriving (Eq)

squareDigit :: Int -> Int
squareDigit n = sum
              . map (sq . snd)
              . reverse
              . takeWhile ((/= 0) . uncurry (+))
              . tail
              . iterate (\(a,_) -> a `quotRem` 10)
              $ (n,0)

-- since we only search in range 1..9,999,999
-- we can preprocess numbers in 1.. 7 * 9^2
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
                    return res'
                else return res
    mapM_ touch [1..upBound]
    return mary

answer :: Int
answer = length
       . filter ((== StuckIn89) . (arriveTable !) . squareDigit)
       $ [1..9999999]

main :: IO ()
main = print answer
