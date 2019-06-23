import Petbox
import Data.Ratio
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.Primes.Factorisation

import Data.Int
import Data.List
import qualified Data.List.Ordered as LO


uniqPrimes = LO.nub . sort . map (fromIntegral . fst)  . factorise

phi :: Int -> Int
phi n
    | n == 1 = 1
    | isPrime (fromIntegral n) = n - 1
    | otherwise = let uprimes = uniqPrimes $ fromIntegral n
                      numer = product . map (subtract 1) $ uprimes
                      denom = product uprimes
                  in (n `div` denom) * numer

fareySum :: Int -> Int
fareySum n = subtract 1 . sum
           $ map phi [1..n]

-- count number of elements in the portion of [a .. 1%3]
-- in a Farey sequence
fareyPartial :: Int -> Int
fareyPartial 1 = 0
fareyPartial n = fareyPartial (n-1) + length candidates
  where
    candidates = map (% n)
               $ filter ((== 1) . gcd n) [1 .. n `div` 3]

{-
  notice that the Farey sequence is symmetric,
  because it holds that gcd(a,n) = gcd(n-a,n) (for n > a).

  notation: let the Farey sequence be 0%1, a, ... b, 1%3, c, ... d, 1%2, ... e, 1%1

  and that:
  - "fareySum n" counts from a to e
  - "(fareySum n - 1) / 2" counts from a to d    -- (1)
  - "fareyPartial n" counts from a to 1%3        -- (2)
  - what we want is the number of elements from c to d
    (1) - (2) does the trick
-}

main :: IO ()
main = do
    let k = halve $ fareySum 12000 - 1
    print $ k - fareyPartial 12000
