module ProjectEuler.Problem22
  ( problem
  ) where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p22-names.txt" 22 Solved compute

nameWorth :: String -> Int
nameWorth n = getSum $ foldMap toInt n
    where
      toInt x = Sum $ ord x - ord 'A' + 1

compute :: T.Text -> Int
compute raw =
  getSum
  . mconcat . zipWith (\i n -> Sum $ i * nameWorth n) [1..]
  $ sort names
 where
   content = T.unpack raw
   names = read $ concat ["[", content, "]"]
