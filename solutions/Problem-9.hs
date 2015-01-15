main :: IO ()
main = print $ x * y * (1000 - x - y)
  where
    sq n = n * n
    (x,y) = head [ (a,b)
                 | a <- [0..1000 :: Int]
                 , b <- [a+1..1000]
                 , a < b
                 , b < (1000-a-b)
                 , sq a + sq b == sq (1000-a-b)
                 ]
