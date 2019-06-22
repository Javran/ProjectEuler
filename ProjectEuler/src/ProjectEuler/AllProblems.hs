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
import ProjectEuler.Problem7
import ProjectEuler.Problem8
import ProjectEuler.Problem9
import ProjectEuler.Problem10
import ProjectEuler.Problem11
import ProjectEuler.Problem12
import ProjectEuler.Problem13
import ProjectEuler.Problem14
import ProjectEuler.Problem15
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
    , (7, ProjectEuler.Problem7.problem)
    , (8, ProjectEuler.Problem8.problem)
    , (9, ProjectEuler.Problem9.problem)
    , (10, ProjectEuler.Problem10.problem)
    , (11, ProjectEuler.Problem11.problem)
    , (12, ProjectEuler.Problem12.problem)
    , (13, ProjectEuler.Problem13.problem)
    , (14, ProjectEuler.Problem14.problem)
    , (15, ProjectEuler.Problem15.problem)
    , (24, ProjectEuler.Problem24.problem)
    ]
