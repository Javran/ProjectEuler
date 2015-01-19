import Data.Char
import Data.Int
import Petbox

squareRoot :: Int64 -> Int64
squareRoot =  floor . (sqrt :: Double -> Double) . fromIntegral

searchSpaceDiv10 :: [Int64]
searchSpaceDiv10 = filter validEnd [lBound .. uBound]
  where
    -- let's say this perfect square number is n*n = N
    -- the last unknown digit of N must be '0',
    -- because the last digit of N is '0',
    -- to make N a perfect square, n must be divisible by 10
    -- therefore the search space is dramatically reduced
    lBound, uBound :: Int64
    lBound = squareRoot . read $ "10203040506070809"
    uBound = squareRoot . read $ "19293949596979899"
    -- further, since N / 100 ends in '9',
    -- the last digit of n must be either 3 or 7
    validEnd n = (n `mod` 10) `elem` [3,7]

-- check the pattern
valid :: Int64 -> Bool
valid xDiv10 = and (zipWith test xStr [1..])
    where
      xStr = show (xDiv10 * xDiv10)
      test ch ind
            | even ind = True
            | otherwise = (ord ch - ord '0') == (ind + 1) `div` 2

main :: IO ()
main = print . (*10) . firstSuchThat valid $ searchSpaceDiv10
