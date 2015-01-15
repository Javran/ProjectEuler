import Data.Char
import System.IO
import Data.List

nameWorth :: String -> Int
nameWorth n = sum $ map toInt n
    where toInt x = ord x - ord 'A' + 1

main :: IO ()
main = do
    h <- openFile "../data/p22-names.txt" ReadMode
    content <- hGetContents h
    let names = read $ concat ["[", content, "]"] :: [String]
    print $ sum $ zipWith (*) [1..] $ map nameWorth $ sort names
