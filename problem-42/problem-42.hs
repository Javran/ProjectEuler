import System.IO
import Data.Char

wordValue word = sum values
    where
        values = map toValue word
        toValue c = ord c - ord 'A' + 1

triangleNumbers = map snd $ iterate (\(i,f) -> (i+1, i+f+1)) (1,1)

main = do
    h <- openFile "./words.txt" ReadMode
    content <- hGetContents h
    let wordList = read $ "[" ++ content ++ "]" :: [String]
    let valueList = map wordValue wordList
    let rangedTriangleNumbers = takeWhile (<= maximum valueList) triangleNumbers
    print $ length $ filter (`elem` rangedTriangleNumbers) valueList
