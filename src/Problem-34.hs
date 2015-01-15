import Data.Char

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = product [1..x]

verify :: Integer -> Bool
verify x =  sum (map (factorial . fromIntegral . digitToInt) (show x)) == x

solve = sum $ filter verify [3..99999]

main = print solve
