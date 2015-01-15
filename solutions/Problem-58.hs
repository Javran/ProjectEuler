import Data.Numbers.Primes
import Data.Function
import Data.List

lineSE, lineSW, lineNE, lineNW :: [Int]
-- f x = (2x - 1)^2
lineSE = map (\x -> (2 * x - 1)^ (2::Int)) [1..]
-- f x = (2x - 1)^2 - (x-1)*2
lineSW = zipWith (-) lineSE [0,2..]
lineNW = zipWith (-) lineSW [0,2..]
lineNE = zipWith (-) lineNW [0,2..]

-- | number of primes in: first sprial = 0 (1), second spiral = 3 (3,5,7) ...
primeCount :: [Int]
primeCount = map countPrimes $ transpose [lineSE, lineSW, lineNW, lineNE]
    where
        countPrimes = length . filter isPrime

primeRatios :: [Double]
primeRatios = zipWith ((/) `on` fromIntegral) primeCountAcc [1,1+4..]
    where primeCountAcc = tail $ scanl (+) 0 primeCount

main :: IO ()
main = print . head . filter ((< 0.1) . fst) . tail $ zip primeRatios [1 :: Int,3..]
