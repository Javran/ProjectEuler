import qualified Data.Array.Unboxed as A
import Data.Array.ST
import Control.Monad

{-
-- this is the original version

-- all possible combinations sum up to "n" with the
-- first and largest number being "m"
numParts :: Int -> Int -> [ [Int] ]
numParts m n
    | n > m = []
    | n == m = [ [n] ]
    | otherwise = do
        n' <- [1..n]
        xs <- numParts (m-n) n'
        return $  n:xs

countParts :: Int -> [ [Int] ]
countParts x = concatMap (numParts x) [1..x]
-}

limit :: Int
limit = 100

numParts :: Int -> Int -> Int
numParts a b
    | a <  b = 0
    | a == b = 1
    | otherwise = numPartsArray A.! (a,b)
  where
    numPartsArray :: A.UArray (Int,Int) Int
    numPartsArray = runSTUArray $ do
        mary <- newArray ((1,1),(limit,limit)) 0
        mapM_ (\x -> writeArray mary (x,x) 1) [1..limit]
        forM_ [1..limit] $ \ m ->
            forM_ [1..m-1] $ \n -> do
                xs <- mapM (\x -> readArray mary (m-n,x)) [1..n]
                writeArray mary (m,n) (sum xs)
        return mary

solve :: Int -> Int
solve x = sum (map (numParts x) [1..x]) - 1

main :: IO ()
main = print $ solve 100
