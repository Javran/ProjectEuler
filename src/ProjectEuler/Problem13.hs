{-# LANGUAGE TypeApplications #-}
module ProjectEuler.Problem13
  ( problem
  ) where

import qualified Data.Text as T

import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p013_numbers.txt" 13 Solved compute

compute :: T.Text -> Integer
compute =
  read . take 10 . show . sum
  . map (read @Integer . T.unpack) . T.lines

