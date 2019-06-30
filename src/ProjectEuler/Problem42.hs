module ProjectEuler.Problem42
  ( problem
  ) where

import Data.Char
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p42-words.txt" 42 Solved compute

wordValue :: String -> Int
wordValue word = sum $ toValue <$> word
  where
    toValue c = ord c - ord 'A' + 1

triangleNumbers :: [Int]
triangleNumbers = snd <$> iterate (\(i,f) -> (i+1, i+f+1)) (1,1)

compute :: T.Text -> Int
compute raw = length $ filter (`elem` rangedTriangleNumbers) valueList
  where
    wordList = read $ "[" ++ T.unpack raw ++ "]"
    valueList = wordValue <$> wordList
    vMax = maximum valueList
    rangedTriangleNumbers = takeWhile (<= vMax) triangleNumbers
