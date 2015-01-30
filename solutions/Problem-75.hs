{-# LANGUAGE TupleSections #-}
import Control.Monad
import qualified Data.Array.Unboxed as A

limit :: Int
limit = 750000

searchSpace :: [Int]
searchSpace = do
    m <- [2..limit]
    mpn <- [m+1..limit `quot` m]
    let n = mpn - m
        halfPeri = m * mpn
    guard $ m > n
         && 1 == gcd m n
         && mpn `rem` 2 == 1
    [halfPeri,halfPeri+halfPeri..limit]

solutions :: [Int]
solutions = filter (== 1) $ A.elems countTable
  where
    countTable :: A.UArray Int Int
    countTable = A.accumArray (+) 0 (1,limit) . map (,1) $ searchSpace

main :: IO ()
main = print $ length solutions
