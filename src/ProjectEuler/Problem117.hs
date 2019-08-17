module ProjectEuler.Problem117
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 117 Unsolved result

{-
  Despite that this problem is based on Problem116,
  I doubt how much code we can reuse, as now tiles of
  diffferent lengths are allowed to mix.

  `ballsInBins` could still be useful,
  but if we were to use it, we need to find all ways
  of constructing up to n tiles using tiles of length 2,3,4,
  and then fill in spaces.
 -}

result = ()


