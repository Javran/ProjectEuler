import Data.Numbers.Primes
import Data.Function (on)
import Data.List
import Control.Monad

-- TODO: things after problem 27 are migrated without too much refactoring

consePrimeLen :: Int -> Int -> Int
consePrimeLen a b = length $ takeWhile isPrime [ n * n + a * n + b | n <- [0..] ]

main :: IO ()
main = do
    let limitedPrimes = takeWhile (<1000) primes
        searchSpace = do
          a <- [0..1000 :: Int]
          b <- limitedPrimes
          guard $ gcd a b == 1
          c <-[-a,a]
          d <-[-b,b]
          guard $ c + d > 0
          return (c,d)
    print $ uncurry (*) $ maximumBy (compare `on` uncurry consePrimeLen) searchSpace
