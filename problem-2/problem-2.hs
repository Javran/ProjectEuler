fibs :: [Int]
fibs = map snd $ iterate (\(x,y) -> (y,x+y)) (1,1)

main = print $ sum $ filter even $ takeWhile (<=4000000) fibs
