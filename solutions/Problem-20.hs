import Data.Char

main :: IO ()
main = print
     . sum . map digitToInt . (show :: Integer -> String)
     $ product [1..100]
