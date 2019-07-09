module ProjectEuler.Problem96
  ( problem
  ) where

import ProjectEuler.Types

import qualified Data.Text as T

{-
  plan: Sudoku has been a well-studied problem for a while,
  I wouldn't mind being fancy and use http://hackage.haskell.org/package/set-cover
 -}

problem :: Problem
problem = pureProblemWithData "p096_sudoku.txt" 96 Unsolved compute

compute :: T.Text -> T.Text
compute = id
