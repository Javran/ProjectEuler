{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Function

import Petbox

{-
  5-gon encoding:

  - the outer ring is a1 a2 a3 a4 a5
  - the inner ring is b1 b2 b3 b4 b5

  the number-encoding is the string concatenation of:
  a1 b1 b2; a2 b2 b3; a3 b3 b4; a4 b4 b5; a5 b5 b1
-}

pickOne :: [a] -> [ [a] ]
pickOne xs = map (take l)
           . take l
           . iterate tail
           $ cycle xs
  where
    l = length xs

pickOne' :: StateT [a] [] a
pickOne' = do
    (x:xs) <- (lift . pickOne) =<< get
    put xs
    return x

solutions :: StateT [Int] [] [Int]
solutions = do
    outer@[a1,a2,a3,a4,a5] <- replicateM 5 pickOne'
    guard $ 10 `elem` outer
    b1 <- pickOne'
    b2 <- pickOne'
    let correctSum = guard . (== a1 + b1 + b2)
    b3 <- pickOne'
    correctSum $ a2 + b2 + b3
    b4 <- pickOne'
    correctSum $ a3 + b3 + b4
    b5 <- pickOne'
    correctSum $ a4 + b4 + b5
    correctSum $ a5 + b5 + b1
    return $ encodeResult [[a1,b1,b2]
                          ,[a2,b2,b3]
                          ,[a3,b3,b4]
                          ,[a4,b4,b5]
                          ,[a5,b5,b1]
                          ]
  where
    -- clockwise, lowest external node
    encodeResult arms = head [ concat enc
                             | enc@(firstArm:restArms) <- pickOne arms
                             , all ((> head firstArm) . head) restArms
                             ]

main :: IO ()
main = print
     . maximumBy (compare `on` snd)
     . map (keepInput concatInt)
     $ evalStateT solutions [1..10]
  where
    concatInt :: [Int] -> Int
    concatInt = read . concatMap show
