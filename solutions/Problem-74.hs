{-# LANGUAGE TupleSections #-}
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Control.Monad
import Petbox
import Data.Maybe

digitFac :: Int -> Int
digitFac = sum . map factorial . allDigits

-- take combinations of 0!, 1!, ... 9!, 6 times.
-- note that since 0! = 1, we need to remove prefixing zeros before
-- taking the sum
searchSpace :: IM.IntMap Int
searchSpace = IM.fromListWith (+) (map ((,1) . takeSum) (replicateM 6 [0..9]))
  where
    takeSum = sum . map factorial . dropWhile (== 0)

-- loop must exist, no need for Maybe
-- modified from Problem 64
-- returns (<the loop>, <reversed non-repeating sequence>)
findLoop :: Ord a => [a] -> ([a],[a])
findLoop = findLoop' []
  where
    findLoop' _ [] = error "no loop detected"
    findLoop' s (y:ys)
        | y `elem` s = (dropWhile (/= y) . reverse $ s, s)
        | otherwise  = findLoop' (y:s) ys

main :: IO ()
main = do
    let valid n = 60 == 1 + ( length
                            . snd
                            . findLoop
                            $ iterate digitFac n)
        -- the second element in chain
        sndInChains = filter valid $ IS.toList $ IM.keysSet searchSpace
    print $ sum $ mapMaybe (`IM.lookup` searchSpace) sndInChains
