module ProjectEuler.Problem52
  ( problem
  ) where

import Data.List
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 52 Solved result

allEqual :: (Eq e) => [e] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual [] = True

isPermNum :: Int -> Bool
isPermNum n = allEqual $ map (sort . show . (* n') ) [1..6]
    where
      n' = oneInFront n

-- the number have to start with `1`
oneInFront :: Int -> Int
oneInFront n = read $ '1' : show n

result :: Int
result = oneInFront $ head $ filter isPermNum [1..]

