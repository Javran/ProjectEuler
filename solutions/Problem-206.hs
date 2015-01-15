import Data.Char
import Data.Int

-- from: http://www.haskell.org/haskellwiki/Generic_number_type#squareRoot

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

space :: [Int64]
space = [1010101010..1389026624] 

main = print $ squareRoot $ fromIntegral $ head $ filter f $ map (^2) space

f x = (length xStr == length "1_2_3_4_5_6_7_8_9_0") && and (zipWith test xStr [1..])
    where
        xStr = show x
        test ch ind
            | even ind = True
            | ind == 19 = ch == '0'
            | otherwise = (ord ch - ord '0') == (ind + 1) `div` 2

{-
compile with: ghc -O
real    2m26.044s
user    2m25.843s
sys     0m0.317s
-}
