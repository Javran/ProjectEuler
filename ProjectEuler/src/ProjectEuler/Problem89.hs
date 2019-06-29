module ProjectEuler.Problem89
  ( problem
  ) where

import Control.Applicative
import Data.Functor
import Petbox
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p89-roman.txt" 89 Solved compute

parseRoman :: Parser Int
parseRoman = sum <$> many1 parseUnit
  where
    parseUnit =
      try (string "IV" $> 4)
      <|> try (string "IX" $> 9)
      <|> try (string "XL" $> 40)
      <|> try (string "XC" $> 90)
      <|> try (string "CD" $> 400)
      <|> try (string "CM" $> 900)
      <|> string "I" $> 1
      <|> string "V" $> 5
      <|> string "X" $> 10
      <|> string "L" $> 50
      <|> string "C" $> 100
      <|> string "D" $> 500
      <|> string "M" $> 1000

toRomanRep :: Int -> String
toRomanRep n
    | n <= 0 || n > 4999 = error "invalid input"
    | otherwise =
        concat
          [ concat (replicate a "M")
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
    digitToStr _ _ _ _ = error "unreachable"

compute :: T.Text -> Int
compute raw =  length (concat rs) - length simpleRs
  where
    rs = lines . T.unpack $ raw
    simpleRs = concatMap (toRomanRep . parseR) rs
    parseR s = v
      where
        Right v = parse parseRoman "Roman" s

