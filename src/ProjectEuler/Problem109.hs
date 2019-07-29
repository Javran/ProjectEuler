module ProjectEuler.Problem109
  ( problem
  ) where

import Data.List
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 109 Unsolved result

{-
  One of those annoying problems that deals with random rule.

  Dynamic programming could work, but find a way to encode
  those states could be trouble.

  What I have in mind for now is to try all
  moves in a sorted way (since permutations are considered
  the same unless it's the last move) and see how far
  can a straightforward search go.
 -}

type Move = (Int, Int)

{-
  all possible moves in descending order of score
 -}
moves :: [(Int, Move)]
moves =
    sortBy cmp
      $ (25,(25,1)) : (50,(25,2)) : [(a*b,(a,b)) | a <- [1..20], b <- [1..3]]
  where
    -- compare first by descending order of score, then by the structure encoding
    -- which move are we representing.
    cmp (x,xm) (y,ym) = compare y x <> compare xm ym

{- one can only finish with a double -}
lastMoves :: [(Int,Move)]
lastMoves = filter ((== 2) . snd . snd) moves

result = ()

