{-
  DO NOT EDIT: this file is generated/edited by templater.
 -}
module ProjectEuler.AllProblems
  ( allProblems
  ) where

import qualified Data.IntMap.Strict as IM

import ProjectEuler.Types

import ProjectEuler.Problem1
import ProjectEuler.Problem2
import ProjectEuler.Problem3
import ProjectEuler.Problem4
import ProjectEuler.Problem5
import ProjectEuler.Problem6
import ProjectEuler.Problem24

allProblems :: IM.IntMap Problem
allProblems =
  IM.fromList
    [ (1, ProjectEuler.Problem1.problem)
    , (2, ProjectEuler.Problem2.problem)
    , (3, ProjectEuler.Problem3.problem)
    , (4, ProjectEuler.Problem4.problem)
    , (5, ProjectEuler.Problem5.problem)
    , (6, ProjectEuler.Problem6.problem)
    , (24, ProjectEuler.Problem24.problem)
    ]
