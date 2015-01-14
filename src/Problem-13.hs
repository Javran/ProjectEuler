import qualified System.IO.Strict as SIO
import Control.Applicative

solve :: String -> String
solve = take 10 . show . sum
      . map (read :: String -> Integer)  . lines

main :: IO String
main = solve <$> SIO.readFile "../data/p13-numbers.txt"
