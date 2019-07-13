module ProjectEuler.Problem67
  ( problem
  ) where

import Data.Ix

import qualified Data.Text as T

import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p067_triangle.txt" 67 Solved compute

-- let's do a line-by-line fold
--   acc = first line = [a1], i = next line, [a2, a3]
--   a2 -> examine positiion in acc: -1 and 0
--   a3 -> examine positiion in acc: 0 and 1
solveMax :: [[Int]] -> Int
solveMax (t:ts) = foldl max 0 bottomLine
    where
        bottomLine = foldl solveNextLine t ts
        solveNextLine curLine nextTableLine = map possibleMax nextTableLinePos
            where
                nextTableLinePos = zip [0..] nextTableLine
                possibleMax (pos,val) = val + foldl max 0 possibleVal
                    where
                        possiblePos = filter valid [pos - 1, pos]
                        possibleVal = map (curLine !!) possiblePos
                        valid = inRange (0, length curLine -1)
solveMax [] = error "unreachable"

compute :: T.Text -> Int
compute raw = solveMax table
  where
    table = map (map read . words) $ lines (T.unpack raw)

