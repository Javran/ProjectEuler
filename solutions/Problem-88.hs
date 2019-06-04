{-# LANGUAGE FlexibleContexts  #-}
import Control.Monad
import Petbox
import qualified Data.IntSet as IS
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST as A

-- let's do it this way:
-- figure out all the ways to divide one number to a product-sum number
-- (if this is not possible, an empty list will be produced
-- so we don't actually need to worry about it)
-- go through all the numbers in [1..12000*9]
-- with proper methods the performance is not an issue

{-
-- "numSplit maxN n n" splits "n" by using numbers
-- less or equal to "maxN"
numSplit :: Int -> Int -> Int -> [ [Int] ]
numSplit 1 num1 1 = [replicate num1 1] -- all the rests are "1"s
numSplit 1 _ _ = [] -- there is no solution if
                    -- we are only allowed to use "1",
                    -- but the product is not yet "1"
numSplit maxN num1 num2 =
    [ [num1] |  num1 == num2 && num1 <= maxN ]
    ++ do
      x <- [1..maxN]
      guard $ num2 `mod` x == 0 && num1 - x > 0
      xs <- numSplit x (num1 - x) (num2 `div` x)
      [x : xs]
-}

-- same as "numSplitN" but only take the length of each results
-- doesn't actually construct the number partitiion so
-- this is supposed to be slightly faster
numSplitN :: Int -> Int -> Int -> [ Int ]
numSplitN 1 num1 1 = [num1]
numSplitN 1 _ _ = []
numSplitN maxN num1 num2 =
    [ 1 |  num1 == num2 && num1 <= maxN ]
    ++ do
      x <- [1..maxN]
      guard $ num2 `mod` x == 0 && num1 - x > 0
      xs <- numSplitN x (num1 - x) (num2 `div` x)
      return (succ xs)

getMinProductNums :: [(Int, [Int])] -> A.UArray Int Int
getMinProductNums partLens = A.runSTUArray $ do
        -- "0" is the uninitialized value,
        -- not in range, nor a solution thus safe
        let bd = (2,12000 :: Int)
            remaining1 = IS.fromList [2..12000]
        mary <- A.newArray bd (0 :: Int)
        let update i _ | not (A.inRange bd i) = return ()
            update i vNew = A.writeArray mary i vNew

            work rmn ((num,ls):xs)
                | IS.null rmn = return ()
                | otherwise = do
                    mapM_ (`update` num) . filter (`IS.member` rmn) $ ls
                    work (foldr IS.delete rmn ls) xs
        work remaining1 partLens
        return mary

-- TODO: try DP, would be faster

main :: IO ()
main = print
     $ sum
     $ IS.toList
     $ IS.fromList
     $ A.elems
     $ getMinProductNums partititionLengths
  where
    partititionLengths = map (keepInput $ \x -> numSplitN x x x) [1..]
