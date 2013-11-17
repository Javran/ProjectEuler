import Data.Numbers.Primes
import Data.Function (on)
import Data.List
import Control.Monad

consePrimeLen a b = length $ takeWhile isPrime [ n * n + a * n + b | n <- [0..] ]

main = do
    let searchSpace = do
        a <- [0..1000]
        b <- takeWhile (<1000) primes
        guard $ gcd a b == 1
        c <-[-a,a]
        d <-[-b,b]
        guard $ c + d > 0
        return (c,d)

    print $ uncurry (*) $ maximumBy (compare `on` uncurry consePrimeLen) searchSpace

{-
compile with: ghc -O2 problem-27.hs -rtsopts
run with: ./problem-27 +RTS -K200000000 -RTS
real    0m19.284s
user    0m18.859s
sys     0m0.384s
-}
