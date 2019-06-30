{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem13
  ( problem
  ) where

import ProjectEuler.Types
import qualified Data.Text as T

problem :: Problem
problem = pureProblemWithData "p13-numbers.txt" 13 Solved compute

compute :: T.Text -> Integer
compute =
  read . take 10 . show . sum
  . map (read @Integer . T.unpack) . T.lines

