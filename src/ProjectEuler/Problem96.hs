module ProjectEuler.Problem96
  ( problem
  ) where

import Control.Monad
import Data.List (intersperse)
import Data.List.HT (sliceVertical)

import qualified Math.SetCover.Exact as ESC
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Array

import ProjectEuler.Types

{-
  plan: Sudoku has been a well-studied problem for a while,
  I wouldn't mind being fancy and use http://hackage.haskell.org/package/set-cover
 -}

problem :: Problem
problem = pureProblemWithData "p096_sudoku.txt" 96 Unsolved (const result)

compute :: T.Text -> T.Text
compute = id

-- following are copied from set-cover examples.

data X = Pos Int Int | Row Int Int | Column Int Int | Square Int Int Int
         deriving (Eq, Ord, Show)

type Assign = ESC.Assign ((Int, Int), Int)

assign :: Int -> Int -> Int -> Assign (Set.Set X)
assign k i j =
   ESC.assign ((i,j), k) $
   Set.fromList [Pos i j, Row k i, Column k j, Square k (div i 3) (div j 3)]

assigns :: [Assign (Set.Set X)]
assigns = liftM3 assign [1..9] [0..8] [0..8]

exampleHawiki1 :: [String]
exampleHawiki1 =
   "    6  8 " :
   " 2       " :
   "  1      " :
   " 7    1 2" :
   "5   3    " :
   "      4  " :
   "  42 1   " :
   "3  7  6  " :
   "       5 " :
   []

stateFromString ::
   (ESC.Set set) =>
   [Assign set] -> [String] -> ESC.State ((Int, Int), Int) set
stateFromString asgns css =
   foldl (flip ESC.updateState) (ESC.initState asgns) $
   do let asnMap = foldMap (\asn -> Map.singleton (ESC.label asn) asn) asgns
      (i,cs) <- zip [0..] css
      (j,c)  <- zip [0..] cs
      guard $ c/=' '
      return $
         Map.findWithDefault
            (error "coordinates not available")
            ((i,j), fromEnum c - fromEnum '0') asnMap

format :: [((Int, Int), Int)] -> String
format =
   unlines . map (intersperse ' ') . sliceVertical 9 . Array.elems .
   fmap (\n -> toEnum $ n + fromEnum '0') .
   Array.array ((0,0),(8,8))


result = unlines $
  format <$> (ESC.search $
              stateFromString assigns exampleHawiki1)
