import Data.Char
import ProjectEuler.Javran
import Data.List

nameWorth :: String -> Int
nameWorth n = sum $ map toInt n
    where toInt x = ord x - ord 'A' + 1

main :: IO ()
main = do
    content <- getDataFile "p22-names.txt"
    let names = read $ concat ["[", content, "]"] :: [String]
    print $ sum $ zipWith (*) [1..] $ map nameWorth $ sort names
