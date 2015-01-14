{-
compile with -O

real    0m1.020s
user    0m1.020s
sys     0m0.000s
-}

divSum :: Int -> Int
divSum x = sum [f | f <- [1..x-1], x `mod` f == 0]

isAmicablePair :: (Int, Int) -> Bool
isAmicablePair (x,y) = (x == divSum y) && (y == divSum x)

main = do
    let ans = [ (x,y) | x <- [1..10000], let y = divSum x, isAmicablePair (x,y), x < y]
    print $ sum ( let getSum (x,y) = x + y in [ getSum t | t <- ans] )
