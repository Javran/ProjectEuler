module ProjectEuler.Problem13
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p13-numbers.txt" 13 Solved compute

compute :: String -> Integer
compute =
  read . take 10 . show . sum
  . map (read :: String -> Integer)  . lines

