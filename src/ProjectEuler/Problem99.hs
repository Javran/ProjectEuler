module ProjectEuler.Problem99
  ( problem
  ) where

import Data.List
import Data.Ord

import qualified Data.Text as T

import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p099_base_exp.txt" 99 Solved compute

compute :: T.Text -> Int
compute = fst . maximumBy (comparing snd) . getNums

getNums :: T.Text -> [(Int,Double)]
getNums = zipWith go [1..] . lines . T.unpack
  where
    go lineN rawLine = (lineN, v)
      where
        (a,b) = read $ "(" ++ rawLine ++ ")" :: (Double,Double)
        v = b * log a


