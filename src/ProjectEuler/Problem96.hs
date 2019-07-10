module ProjectEuler.Problem96
  ( problem
  ) where

import Control.Monad
import Data.List.Split
import Data.Char

import qualified Math.SetCover.Exact as ESC
import qualified Data.Text as T
{-
  the decision to stick with Set instead of more efficient bit vector,
  is to make sure that we work with some interface that we are familiar with
  so we can more easily develop our own version of the solver.
 -}
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Array as Array

import ProjectEuler.Types

{-
  TODO: the problem is solved with set-cover,
  but I actually wonder if we can do better than this
  if we actually write the solving logic ourselves.
 -}

problem :: Problem
problem = pureProblemWithData "p096_sudoku.txt" 96 Solved compute

type Puzzle = [[Int]]

parsePuzzles :: T.Text -> [Puzzle]
parsePuzzles = fmap parsePuzzle . chunksOf 10 . T.lines
  where
    parsePuzzle :: [T.Text] -> Puzzle
    parsePuzzle (_h:xs) = fmap (T.foldr (\c r -> (ord c - ord '0') : r) []) xs
    parsePuzzle _ = error "Unexpectd extra lines"

compute :: T.Text -> Int
compute =
  sum
  . fmap solve
  . parsePuzzles

data X
  = Pos Int Int
  | Row Int Int
  | Column Int Int
  | Square Int Int Int
  deriving (Eq, Ord)

type Assign = ESC.Assign ((Int, Int), Int)

assign :: Int -> Int -> Int -> Assign (Set.Set X)
assign k i j =
   ESC.assign ((i,j), k) $
   Set.fromList [Pos i j, Row k i, Column k j, Square k (div i 3) (div j 3)]

assigns :: [Assign (Set.Set X)]
assigns = liftM3 assign [1..9] [0..8] [0..8]

stateFromString ::
   (ESC.Set set) =>
   [Assign set] -> Puzzle -> ESC.State ((Int, Int), Int) set
stateFromString asgns css =
   foldl (flip ESC.updateState) (ESC.initState asgns) $
   do let asnMap = foldMap (\asn -> Map.singleton (ESC.label asn) asn) asgns
      (i,cs) <- zip [0..] css
      (j,c)  <- zip [0..] cs
      guard $ c /= 0
      pure $
        Map.findWithDefault
          (error "coordinates not available")
          ((i,j), c) asnMap

solve :: Puzzle -> Int
solve inp =
  (\[a,b,c] -> a*100 + b*10 +c)
  . take 3
  . Array.elems
  . Array.array ((0,0),(8,8))
  . head
  . ESC.search
  $ stateFromString assigns inp
