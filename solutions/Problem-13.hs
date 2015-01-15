import Control.Applicative
import ProjectEuler.Javran

solve :: String -> String
solve = take 10 . show . sum
      . map (read :: String -> Integer)  . lines

main :: IO String
main = solve <$> getDataFile "p13-numbers.txt"
