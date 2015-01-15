import Data.Numbers.Primes
import Control.Monad

-- calculate sum of primes
--   keep the first 0 so that the sum of all primes
--   from p[1] to p[n], is just sum_p[n] - sum_p[0] = sum_p[n]
primeSum = 0 : zipWith (+) primeSum primes

-- only consider possible sums in the range
primeSumRanged = takeWhile (<=1000000) primeSum

consecPrimeSum = do
    let lenMax = length primeSumRanged
    len <- [lenMax  ,lenMax-1..1]
    i   <- [lenMax-1,lenMax-2..0]
    let j = i - len + 1
    guard $ 0 <= j && j < lenMax
    -- now we have i and j
    --   let's enumerate all possible consecutive sums
    --   random-access list support will be great
    --   but the search space isn't too scary.
    return $ primeSumRanged !! i - primeSumRanged !! j

main = print $ head $ filter isPrime consecPrimeSum
