divSum :: Int -> Int
divSum x = sum [f | f <- [1..x-1], x `mod` f == 0]

-- TODO: get factors?
isAmicablePair :: (Int, Int) -> Bool
isAmicablePair (x,y) = (x == divSum y) && (y == divSum x)

main :: IO ()
main = do
    let ans = [ (x,y) | x <- [1..10000], let y = divSum x, x < y, isAmicablePair (x,y)]
    print $ sum $ map (uncurry (+)) ans
