import ProjectEuler.Javran
import Control.Applicative
import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A
import Data.Ix
import Control.Monad
import Petbox

getMat :: IO (A.UArray (Int,Int) Int)
getMat = do
    raws <- getRaws
    let colN = length (head raws)
        rowN = length raws
        genPairs = concat $ add2DCoords 1 1 raws
    return $ A.array ((1,1), (rowN,colN)) genPairs
  where
    getRaws = map parseLine . lines <$> getDataFile "p81-matrix.txt"
    parseLine s = read ("[" ++ s ++ "]") :: [Int]

pathSum :: A.UArray (Int,Int) Int -> A.UArray (Int,Int) Int
pathSum mat = A.runSTUArray $ do
    mary <- A.newArray bd 0
    let safeAccess c
            | inRange bd c = A.readArray mary c
            | otherwise    = return 0
    A.writeArray mary (1,1) (mat A.! (1,1))
    forM_ [1..rowN] $ \i ->
        forM_ [1..colN] $ \j -> do
            fromUp <- safeAccess (i-1,j)
            fromLeft <- safeAccess (i,j-1)
            let candidates = [fromUp | i-1 >= 1] ++ [fromLeft | j-1>=1]
            unless (i == 1 && j == 1)
                (A.writeArray mary (i,j) (mat A.! (i,j) + minimum candidates))
    return mary
  where
    bd@(_,(rowN,colN)) = A.bounds mat

main :: IO ()
main = getMat >>= print . getResult . pathSum
  where
    getResult arr = arr A.! snd (A.bounds arr)
