{-# LANGUAGE TupleSections #-}
import Control.Monad
import qualified Data.Array.Unboxed as A

-- see: http://en.wikipedia.org/wiki/Pythagorean_triple
-- when m,n are coprimes and (m-n) is odd,
-- the formula: a = m^2 + n^2, b = 2 * m * n, c = m^2 - n^2
-- will generate Pythagorean triples
-- the perimeter is a+b+c = 2 * k * m * (m+n)
-- we simplify the constraint 1,500,000 >= 2 * k * m * (m+n)
-- to: 750,000 >= k * m * (m+n)
-- we can enumerate all possible combinations and count each of them

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
         && odd mpn -- if m + n is odd, then so does m - n
    [halfPeri,halfPeri+halfPeri..limit]

solutions :: [Int]
solutions = filter (== 1) $ A.elems countTable
  where
    countTable :: A.UArray Int Int
    countTable = A.accumArray (+) 0 (1,limit) . map (,1) $ searchSpace

main :: IO ()
main = print $ length solutions
