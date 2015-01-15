import Data.Numbers.Primes

n :: Integer
n = 600851475143

main :: IO ()
main = print
     . head . filter (\x -> n `mod` x == 0)
     . reverse
     . takeWhile (\x -> x*x <= n)
     $ primes
