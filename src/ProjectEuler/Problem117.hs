module ProjectEuler.Problem117
  ( problem
  ) where

import ProjectEuler.Types
import Data.Monoid
import Control.Monad

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

-- try brute force the combinations - which is too slow.
search :: Int -> [[Int]]
search emptyCount = do
  y <- [2,3,4]
  guard $ y <= emptyCount
  [[y]] <> ((y:) <$> search (emptyCount - y))

result = search 10


