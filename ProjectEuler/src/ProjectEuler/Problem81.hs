module ProjectEuler.Problem81
  ( problem
  ) where

import Control.Monad
import Data.Ix
import Petbox

import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p81-matrix.txt" 81 Solved compute

getMat :: T.Text -> A.UArray (Int,Int) Int
getMat raw = A.array ((1,1), (rowN,colN)) genPairs
  where
    raws = map parseLine . lines . T.unpack $ raw
    parseLine s = read ("[" ++ s ++ "]") :: [Int]
    colN = length (head raws)
    rowN = length raws
    genPairs = concat $ add2DCoords 1 1 raws

pathSum :: A.UArray (Int,Int) Int -> A.UArray (Int,Int) Int
pathSum mat = A.runSTUArray $ do
    mary <- A.newArray bd 0
    let safeAccess c =
          if inRange bd c
            then A.readArray mary c
            else pure 0
    A.writeArray mary (1,1) (mat A.! (1,1))
    forM_ [1..rowN] $ \i ->
        forM_ [1..colN] $ \j -> do
            fromUp <- safeAccess (i-1,j)
            fromLeft <- safeAccess (i,j-1)
            let candidates = [fromUp | i-1 >= 1] ++ [fromLeft | j-1>=1]
            unless (i == 1 && j == 1)
                (A.writeArray mary (i,j) (mat A.! (i,j) + minimum candidates))
    pure mary
  where
    bd@(_,(rowN,colN)) = A.bounds mat

compute :: T.Text -> Int
compute = getResult . pathSum . getMat
  where
    getResult arr = arr A.! snd (A.bounds arr)

