import Data.Numbers.Primes

n = 600851475143

main = print $ head $ filter (\x -> n `mod` x == 0) $ reverse $ takeWhile (\x -> x*x <= n) primes
