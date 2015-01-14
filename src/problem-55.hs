isPalindrome :: Integer -> Bool
isPalindrome x = show x == (reverse . show) x

-- | reverse an integer (hopefully this might be a little more efficient)
reverseInt :: Integer -> Integer
reverseInt x = foldl (\acc i -> acc * 10 + i) 0 remainders
    where
        remainders = map snd
                   . tail
                   . takeWhile (\(a,b) -> not (a == 0 && b == 0))
                   . iterate (\(q,_) -> q `divMod` 10)
                   $ (x,0)

isLychrel :: Integer -> Bool
isLychrel x = not . any isPalindrome . take 50 . tail $ iterate next x
    where next y = y + reverseInt y

main :: IO ()
main = do
    print . length . filter isLychrel $ [1..10000-1]
    print $ isLychrel 4994

