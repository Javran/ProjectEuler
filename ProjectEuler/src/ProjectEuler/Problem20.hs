{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem20
  ( problem
  ) where

import Data.Char
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 20 Solved result

result :: Int
result =
  sum . map digitToInt . show @Integer
  $ product [1..100]

