{-
  DO NOT EDIT: this file is generated/edited by templater.
 -}
module ProjectEuler.AllProblems
  ( allProblems
  ) where

import qualified Data.IntMap.Strict as IM

import ProjectEuler.Types

import ProjectEuler.Problem1
import ProjectEuler.Problem3
import ProjectEuler.Problem24

allProblems :: IM.IntMap Problem
allProblems =
  IM.fromList
    [ (1, ProjectEuler.Problem1.problem)
    , (3, ProjectEuler.Problem3.problem)
    , (24, ProjectEuler.Problem24.problem)
    ]
