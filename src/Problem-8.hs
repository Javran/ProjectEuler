import Control.Applicative
import qualified System.IO.Strict as SIO

rawData :: IO String
rawData = concat . lines <$> SIO.readFile "../data/p8.txt"

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs = take (l-n+1)
                    . map (take n)
                    . iterate tail
                    $ xs
  where
    l = length xs

solve :: Int -> String -> Integer
solve n raw = maximum (map product (slidingWindows n parsed))
  where
    parsed :: [Integer]
    parsed = map (read . (:[])) raw

main :: IO ()
main = rawData >>= print . solve 13
