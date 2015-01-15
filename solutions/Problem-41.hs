import Data.Numbers.Primes
import Petbox

main = do
    let possiblePrimes len = filter isPrime $ map digitsToInt $ reverse $ permutations [1..len]
    let possiblePandigitalPrimes = concatMap possiblePrimes [9,8..1]
    print $ head possiblePandigitalPrimes
