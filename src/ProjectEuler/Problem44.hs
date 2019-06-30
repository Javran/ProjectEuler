module ProjectEuler.Problem44
  ( problem
  ) where

import Control.Monad
import qualified Data.IntSet as IS
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 44 Solved result

{-
  P(n) = n*(3n-1) / 2
  => P(n) - P(n-1) = 3n-2

  let P'(n) = 3n-2
  => P'(n) - P'(n-1) = 3

 -}
pentagonals :: [Int]
pentagonals = tail $ scanl (+) 0 $ iterate (+ 3) 1

result :: Int
result = head possibleDiffs
  where
    possibleDiffs :: [Int]
    possibleDiffs = do
      (n, pN) <- zip [1..] pentagonals
      let space = IS.fromDistinctAscList $ take (n-1) pentagonals
      p2 <- IS.toList space
      -- p2 + p1 = pN
      let p1 = pN - p2
      guard $ p1 <= p2
      guard $ IS.member p1 space
      let pDiff = p2 - p1
      guard $ IS.member pDiff space
      pure pDiff
