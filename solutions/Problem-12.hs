import Data.Numbers.Primes
import Petbox

triNumber :: Integral a => a -> a
triNumber x = ((x + 1) * x) `div` 2

factors :: Int -> [(Int,Int)]
factors m = filter ((/=0).snd) $ factorsAux m possiblePrimes
    where
        possiblePrimes = takeWhile (\x -> sq x <= m) primes
        factorsAux 1 [] = []
        factorsAux n [] = [(n,1)]
        factorsAux n (p:ps) = (p,times) : factorsAux (n `div` (p^times)) ps
            where
                times = last $ takeWhile (\c -> n `mod` (p^c) == 0) [0..]

divisorCount :: Int -> Int
divisorCount n = product $ map ((+1) . snd) $ factors n

main :: IO ()
main = print
     $ firstSuchThat (\x -> divisorCount x > 500) $ map triNumber [1..]
