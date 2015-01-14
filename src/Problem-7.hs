import Data.Numbers.Primes

main :: IO ()
main = print (primes !! (10001-1) :: Int) -- since list indices are 0-based
