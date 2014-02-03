import Data.Numbers.Primes

odds :: [Int]
odds = 3 : map (+2) odds

squareNumbers :: [Int]
squareNumbers = 1 : zipWith (+) squareNumbers odds

oddCompositeNumbers :: [Int]
oddCompositeNumbers = oddCompAux odds primes
    where
        oddCompAux curOdds@(o:os) curPrimes@(p:ps)
            | p  > o = o:oddCompAux os curPrimes
            | p == o = oddCompAux os ps
            | p  < o = oddCompAux curOdds ps

isSquareNumber :: Int -> Bool
isSquareNumber x = x == y * y
    where y = floor $ sqrt $ fromIntegral x

writtenAsPrimeAnd2Sq n = any primeAnd2Sq rangedPrimes
    where
        primeAnd2Sq p = rem == 0 && isSquareNumber quo
            where
                (quo, rem) = (n - p) `divMod` 2
        rangedPrimes = takeWhile (<= n) primes

main = print $ head $ filter (not . writtenAsPrimeAnd2Sq) oddCompositeNumbers
