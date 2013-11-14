import qualified Data.Set as S
import Data.Numbers.Primes

isTruncatable :: Int -> Bool
isTruncatable x = all isPrime $ possibleNumLeft ++ possibleNumRight
    where
        strX = show x
        possibleNumLeft = map read $ map (\x -> take x strX) [1..length strX]
        possibleNumRight = map read $ map (\x -> drop x strX) [1..length strX-1]

main = do
    let searchSpace = dropWhile (<=7) primes
    print $ sum $ take 11 $ filter isTruncatable searchSpace
