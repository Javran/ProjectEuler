import Math.NumberTheory.Primes
import Petbox hiding (primes)

{-
  according to: http://en.wikipedia.org/wiki/Euler%27s_totient_function

  phi(n) = n * (1-1/p1) * (1-1/p2) ...

  therefore: n / phi(n) = 1 / ( (1-1/p1) * (1-1/p2) ... )

  we can conclude that:

  * the number of times a prime number shows up
    in the factorization of n doesn't matter

  * we need to find as many distinct prime numbers as possible

-}

main :: IO ()
main = print . lastSuchThat (<= 1000000) . scanl1 (*) $ (primes :: [Int])
