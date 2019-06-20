module ProjectEuler.Problem8
  ( problem
  ) where

import ProjectEuler.Types
import Control.Applicative
import ProjectEuler.GetData
import qualified System.IO.Strict as SIO

problem :: Problem
problem = Problem 8 Solved compute

rawData :: IO String
rawData = concat . lines <$> getDataFile "p8.txt"

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

compute :: PEM ()
compute = io rawData >>= logT . solve 13
