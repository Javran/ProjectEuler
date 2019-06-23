import Petbox
import Math.NumberTheory.Primes.Testing

numParts :: Int -> Int -> [ [Int] ]
numParts m n
    | n > m = []
    | n == m = [ [n] | isPrime (fromIntegral m)]
    | not (isPrime (fromIntegral n)) = []
    | otherwise = do
        n' <- takeWhile (<= n) primes
        xs <- numParts (m-n) n'
        return $  n:xs

countParts :: Int -> [ [Int] ]
countParts x = concatMap (numParts x) [1..x]

main :: IO ()
main = print $ firstSuchThat ((> 5000) . length . countParts) [1..]
