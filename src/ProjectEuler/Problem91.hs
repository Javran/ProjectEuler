module ProjectEuler.Problem91
  ( problem
  ) where

import Control.Monad
import Data.List

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 91 Solved result

maxN :: Int
maxN = 50

solutions :: [((Int,Int),(Int,Int))]
solutions = do
    let space = [0..maxN]
        -- first one is (0,0), drop it.
        coords = tail [(x,y) | x <- space, y <- space]
    ((x1,y1):coords2) <- tails coords
    (x2,y2) <- coords2
    let c1 = (x1,y1)
        c2 = (x2,y2)
        aSq = x1 * x1 + y1 * y1
        bSq = x2 * x2 + y2 * y2
        cSq = let m = x1 - x2
                  n = y1 - y2
              in m*m + n*n
        sqSum = aSq + bSq + cSq
        sideSq = maximum [aSq,bSq,cSq]
    guard $ sideSq * 2 == sqSum
    pure (c1,c2)

result :: Int
result = length solutions
