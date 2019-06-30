{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem82
  ( problem
  ) where

import Control.Monad
import Petbox

import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p82-matrix.txt" 82 Solved compute

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
    -- initialize the first col
    forM_ [1..rowN] $ \row ->
        A.writeArray mary (row,1) (mat A.! (row,1))
    forM_ [2..colN-1] $ \col -> do
        -- update from previous col
        forM_ [1..rowN] $ \row -> do
            v <- A.readArray mary (row,col-1)
            A.writeArray mary (row,col) (v + mat A.! (row,col))
        -- update from ups
        forM_ [2..rowN] $ \row -> do
            vUp <- (mat A.! (row,col) +) <$> A.readArray mary (row-1,col)
            v   <- A.readArray mary (row,col)
            A.writeArray mary (row,col) (min v vUp)
        -- update from downs
        forM_ [rowN-1,rowN-2..1] $ \row -> do
            vDn <- (mat A.! (row,col) +) <$> A.readArray mary (row+1,col)
            v   <- A.readArray mary (row,col)
            A.writeArray mary (row,col) (min v vDn)
    -- the last col
    forM_ [1..rowN] $ \row ->
        ((mat A.! (row,colN) +) <$> A.readArray mary (row,colN-1))
        >>= A.writeArray mary (row,colN)
    pure mary
  where
    bd@(_,(rowN,colN)) = A.bounds mat

compute :: T.Text -> Int
compute = getResult . pathSum . getMat
  where
    getResult arr = minimum (map ((arr A.!). (,colN) ) [1..rowN])
      where
        (_,(rowN,colN)) = A.bounds arr

