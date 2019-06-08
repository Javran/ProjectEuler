module ProjectEuler.AllProblems
  ( allProblems
  ) where

import qualified Data.IntMap.Strict as IM

-- This module will be automatically generated in future
-- so that Problem instances are collected here.
import ProjectEuler.Types

import ProjectEuler.Problem1
import ProjectEuler.Problem24

allProblems :: IM.IntMap Problem
allProblems =
  IM.fromList
    [ (1, ProjectEuler.Problem1.problem)
    , (24, ProjectEuler.Problem24.problem)
    ]