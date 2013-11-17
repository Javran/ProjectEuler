import Data.Numbers.Primes
import ProjectEuler.Everything

main = do
    let possiblePrimes len = filter isPrime $ map digitToNum $ reverse $ allPermutation [1..len]
    let possiblePandigitalPrimes = concatMap possiblePrimes [9,8..1]
    print $ head possiblePandigitalPrimes
