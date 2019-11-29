module ProjectEuler.Problem136
  ( problem
  ) where

import Data.Monoid

import qualified Data.Vector.Unboxed as VU

import ProjectEuler.Types
import ProjectEuler.Problem135 (countSameDiffs)

problem :: Problem
problem = pureProblem 136 Unsolved result

result :: Int
result =
  getSum
  . foldMap (\n -> if n == 1 then 1 else 0)
  $ VU.toList
  $ countSameDiffs (50000000-1)

