import Data.Numbers.Primes

triNumber x = ((x + 1) * x) `div` 2

factors n = filter ((/=0).snd)  $ factorsAux n possiblePrimes
    where
        possiblePrimes = takeWhile (\x -> x^2 <= n) primes
        factorsAux 1 [] = []
        factorsAux n [] = [(n,1)]
        factorsAux n (p:ps) = (p,times) : factorsAux (n `div` (p^times)) ps
            where
                times = last $ takeWhile (\c -> n `mod` (p^c) == 0) [0..] 

divisorCount n = product $ map ((+1) . snd) $ factors n

main =
    print $ head $ filter (\x -> divisorCount x > 500) $ map triNumber [1..]

{-
compile with ghc -O

real    0m1.230s
user    0m1.227s
sys     0m0.003s
-}

