import ProjectEuler.Javran
import Data.Array
import Control.Applicative
import Control.Arrow

type Index = (Int, Int)

getGrid :: IO (Array Index Integer)
getGrid = do
    grid <- map (map read . words) . lines <$> getDataFile "p11-grid.txt"
    let rows = length grid
        cols = length (head grid)
        size = (rows,cols)
    return (listArray ((1,1), size) (concat grid))

inGrid :: Index -> Bool
inGrid = inRange ((1,1), (20,20))

rowCoordinates, colCoordinates, dg1Coordinates, dg2Coordinates :: [ [Index] ]
rowCoordinates = [ [ (row,col) | col <- [1..20] ] | row <- [1..20] ]
colCoordinates = [ [ (row,col) | row <- [1..20] ] | col <- [1..20] ]
dg1Coordinates = [ takeWhile inGrid (iterate next (1,headCol)) | headCol <- [20,19..1] ]
              ++ [ takeWhile inGrid (iterate next (headRow,1)) | headRow <- [2..20] ]
  where
    next = succ *** succ
dg2Coordinates = [ takeWhile inGrid (iterate next (1,headCol)) | headCol <- [1..20] ]
              ++ [ takeWhile inGrid (iterate next (headRow,20)) | headRow <- [2..20] ]
  where
    next = succ *** pred

allWindows :: [ [Index] ]
allWindows = concatMap (slidingWindows 4)
                       (  rowCoordinates
                       ++ colCoordinates
                       ++ dg1Coordinates
                       ++ dg2Coordinates
                       )

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs = take (l-n+1)
                    . map (take n)
                    . iterate tail
                    $ xs
  where
    l = length xs

getProduct :: Array Index Integer -> [Index] -> Integer
getProduct ar = product . map (ar !)

main :: IO ()
main = do
    grid <- getGrid
    print (maximum (map (getProduct grid) allWindows))
