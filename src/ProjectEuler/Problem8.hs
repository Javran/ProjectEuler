module ProjectEuler.Problem8
  ( problem
  ) where

import Data.Char

import qualified Data.Text as T

import ProjectEuler.SolCommon
import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p008_product.txt" 8 Solved compute

solve :: Int -> String -> Integer
solve n raw = maximum (map product (slidingWindows n parsed))
  where
    parsed :: [Integer]
    parsed = map (\c -> fromIntegral $ ord c - ord '0') raw

compute :: T.Text -> Integer
compute raw = solve 13 rawData
  where
    rawData = filter (not . isSpace) . T.unpack $ raw
