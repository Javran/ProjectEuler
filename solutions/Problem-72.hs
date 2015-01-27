import Petbox
import Data.Numbers.Primes
import Data.Int
import qualified Data.List.Ordered as LO

-- https://oeis.org/A015614

uniqPrimes :: Integral a => a -> [a]
uniqPrimes = LO.nub . primeFactors

phi :: Int -> Int
phi n
    | n == 1 = 1
    | isPrime n = n - 1
    | otherwise = let uprimes = uniqPrimes n
                      numer = product . map (subtract 1) $ uprimes
                      denom = product uprimes
                  in (n `div` denom) * numer

-- because the approximated result exceeds the guaranteed range
-- an Int can hold, we are using Int64 to make it safe

faySum :: Int -> Int64
faySum n = subtract 1 . sum
         $ ( map (fromIntegral . phi) [1..n]  :: [Int64])

main :: IO ()
main = do
    let n = 1000000
        n' = fromIntegral n
    -- approximated result
    -- see: http://en.wikipedia.org/wiki/Farey_sequence
    print (3 * n' * n' / (pi^!2) :: Double)
    print $ faySum n
