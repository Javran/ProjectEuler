module ProjectEuler.Problem8
  ( problem
  ) where

import Data.Char
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p008_product.txt" 8 Solved compute


slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs =
    take (l-n+1)
    . map (take n)
    . iterate tail
    $ xs
  where
    l = length xs

solve :: Int -> String -> Integer
solve n raw = maximum (map product (slidingWindows n parsed))
  where
    parsed :: [Integer]
    parsed = map (\c -> fromIntegral $ ord c - ord '0') raw

compute :: T.Text -> Integer
compute raw = solve 13 rawData
  where
    rawData = filter (not . isSpace) . T.unpack $ raw
