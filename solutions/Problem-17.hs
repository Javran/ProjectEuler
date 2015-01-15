numLetter :: Int -> String
numLetter x
    | x <= 0  = undefined
    | x <= 9  = -- 1 <= x <= 9
        let wordList = words "one two three four five six seven eight nine"
        in wordList !! (x-1)
    | x <= 19 = -- 10 <= x <= 19
        let wordList = words "ten eleven twelve thirteen fourteen \
                             \fifteen sixteen seventeen eighteen nineteen"
        in wordList !! (x-10)
    | x <= 99 = -- 20 <= x <= 99
        let (hi,lo) = x `divMod` 10
            wordList = words "twenty thirty forty fifty sixty seventy eighty ninety"
        in wordList !! (hi-2) ++ if lo /= 0 then numLetter lo else ""
    | x <= 999 = -- 100 <= x <= 999
        let (hi,lo) = x `divMod` 100
        in numLetter hi ++ "hundred" ++ if lo /= 0 then "and" ++ numLetter lo else ""
    | x == 1000 = "onethousand"
    | otherwise = undefined

main :: IO ()
main = print
     . length
     . concatMap numLetter
     $ [1..1000]
