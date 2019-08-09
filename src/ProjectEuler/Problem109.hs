module ProjectEuler.Problem109
  ( problem
  ) where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Monoid
import Petbox

import qualified Data.IntMap.Strict as IM

import ProjectEuler.SolCommon
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 109 Solved result

{-
  One of those annoying problems that deals with random rule.

  Dynamic programming could work, but find a way to encode
  those states could be troublesome.

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

-- one can only finish with a double.
lastMoves :: [(Int,Move)]
lastMoves = filter ((== 2) . snd . snd) moves

-- dict from remaining score to all moves that can finish the game.
dLastMoves :: IM.IntMap [Move]
dLastMoves =
  IM.fromListWith (<>) . (fmap . second) (:[]) $ lastMoves

-- try to finish the game with exactly one double move.
finishGame :: Int -> [] Move
finishGame score = fromMaybe [] (IM.lookup score dLastMoves)

playWithMoves :: Int -> Int -> [(Int, Move)] -> [] [Move]
playWithMoves 0 _ _ = []
playWithMoves cnt score candidates =
  -- either finish the game right now
  ((:[]) <$> finishGame score)
  <> do
    {-
      Since the moves are sorted, we can just drop some large scores
      that will result in a bust from head to avoid some condition checking.
     -}
    let candidates' = dropWhile ((score <=) . fst) candidates
    {-
      Here we attach the chosen element back to the list,
      this allows the taken element to be used multiple times.
     -}
    ((s,m),b) <- pickInOrder' candidates'
    (m:) <$> playWithMoves (cnt - 1) (score - s) b

result :: Int
result =
  getSum $
    foldMap
      (\s -> foldMap (const 1) (playWithMoves 3 s moves))
      [2..99]

