import Data.Char
import Numeric

checkPalind :: String -> Bool
checkPalind x = x == (reverse x)

toBinString :: (Integral a, Show a) => a -> String
toBinString x = showIntAtBase 2 intToDigit x ""

solve = sum [ x | x<-[1..1000000-1], (checkPalind.toBinString) x, (checkPalind.show) x ]

main = print solve
