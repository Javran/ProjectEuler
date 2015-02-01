{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Data.Monoid
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import ProjectEuler.Javran
import Petbox

parseRoman :: Parser Int
parseRoman = sum <$> many1 parseUnit
  where
    parseUnit =  try (string "IV" *> return    4)
             <|> try (string "IX" *> return    9)
             <|> try (string "XL" *> return   40)
             <|> try (string "XC" *> return   90)
             <|> try (string "CD" *> return  400)
             <|> try (string "CM" *> return  900)
             <|>      string "I"  *> return    1
             <|>      string "V"  *> return    5
             <|>      string "X"  *> return   10
             <|>      string "L"  *> return   50
             <|>      string "C"  *> return  100
             <|>      string "D"  *> return  500
             <|>      string "M"  *> return 1000

toRomanRep :: Int -> String
toRomanRep n
    | n <= 0 || n > 4999 = error "invalid input"
    | otherwise = concat [ concat (replicate a "M")
                         , digitToStr "C" "D" "M" b
                         , digitToStr "X" "L" "C" c
                         , digitToStr "I" "V" "X" d
                         ]
  where
    [_,a,b,c,d] = toDigits (10000 + n)
    digitToStr one five ten x
        | x == 0 = ""
        | x <= 3 = concat $ replicate x one
        | x == 4 = one <> five
        | x == 5 = five
        | x <= 8 = concat (five:replicate (x-5) one)
        | x == 9 = one <> ten

getRomans :: IO [String]
getRomans = lines <$> getDataFile "p89-roman.txt"

main :: IO ()
main = do
    rs <- getRomans
    let simpleRs = concatMap (toRomanRep . parseR) rs
    print $ length (concat rs) - length simpleRs
  where
    parseR s = v
      where
        Right v = parse parseRoman "Roman" s
