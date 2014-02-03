import Data.Numbers.Primes

odds :: Integral a => [a]
odds = map (\x -> x*2+1) [1..]

isSquareNumber :: (Integral a) => a -> Bool
isSquareNumber x = fromIntegral x == y * y
    where y = floor $ sqrt $ fromIntegral x

writtenAsConj n = any primeAnd2Sq rangedPrimes
    where
        primeAnd2Sq p = rem == 0 && isSquareNumber quo
            where
                (quo, rem) = (p - n) `divMod` 2
        rangedPrimes = takeWhile (<= n) primes


main = print $ head $ filter (\x -> (not $ isPrime x) && (writtenAsConj x)) odds
