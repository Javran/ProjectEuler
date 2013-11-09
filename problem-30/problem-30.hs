import Data.Char

testAllow :: Int -> Bool 
testAllow x = sum( map (^5) (map (digitToInt) (show x)) ) == x where

solve = sum( filter (testAllow) [10.. (9^5 * 5)] )

main = print solve
