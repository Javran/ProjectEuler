fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

main :: IO ()
main = print
     . sum . filter even      -- sum of evens
     . takeWhile (<=4000000)  -- do not exceed four million
     $ drop 2 fibs            -- correct the offset
