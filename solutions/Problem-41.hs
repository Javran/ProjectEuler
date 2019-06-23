import Math.NumberTheory.Primes.Testing
import Petbox hiding (isPrime)

main = do
    let possiblePrimes len =
          filter (isPrime . fromIntegral)
            $ map digitsToInt $ reverse $ permutations [1..len]
    let possiblePandigitalPrimes = concatMap possiblePrimes [9,8..1]
    print $ head possiblePandigitalPrimes
