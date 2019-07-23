{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , BangPatterns
  #-}
module ProjectEuler.Problem107
  ( problem
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Function
import Data.List
import Data.Maybe

import qualified Data.Text as T
import qualified Data.UnionFind.ST as UF
import qualified Data.Vector.Mutable as MVec

import ProjectEuler.GetData

{-
  Surely Dijkstra's algorithm should do, so why don't we try something new,
  say Prim's algorithm.

  This will involve:
  - a heap to pop up edges of minimal weights
  - disjoint set so we can easily tell when to stop
    (for now I'm thinking about size-based disjoint set,
    so that we can look at any node and see if the root set has contained
    all nodes as the condition of terminating the algorithm.)
 -}

problem :: Problem
problem = pureProblemWithData "p107_network.txt" 107 Solved compute

type Edge = (Int, Int)

vMax :: Int
vMax = 40

vertices :: [Int]
vertices = [0 .. vMax-1]

prim'sAlgorithm :: [] (Edge, Int) -> Int
prim'sAlgorithm initPsq = runST $ do
  uf <- MVec.unsafeNew vMax
  -- initialize union-find-set, the descriptor we are using here
  -- stands for current size of the equivalent class of that element (vertex).
  forM_ vertices $ \v -> MVec.write uf v =<< UF.fresh 1
  (fix $ \loop !psq !result -> do
      {-
        Looking at first vertex and get # of vertices of the equivalent class
        that this vertex is in.
        The purpose of this step is to see whether all vertices has been connected,
        so actually 0 is a random choice - any vertex should do.
       -}
      !eqCnt <- UF.descriptor =<< MVec.read uf 0
      {-
        If we have already have all vertices connected,
        no more edges are needed to be considered.
        (as the edges are sorted in the ascending order of weights)
       -}
      if eqCnt == vMax
        then pure result
        else
          case uncons psq of
            Nothing ->
              -- just in case that we run out of edges to try.
              pure result
            Just (((!x,!y), !w), psq') -> do
              !px <- MVec.read uf x
              !py <- MVec.read uf y
              connected <- UF.equivalent px py
              if connected
                then
                  loop psq' result
                else do
                  -- update descriptor to keep in sync with the merge.
                  UF.union' px py $ \u v -> pure $! u + v
                  loop psq' $! result + w
    ) initPsq 0

compute :: T.Text -> Int
compute raw =
    {-
      Here notice that we are actually using sorting from Data.List, rather than
      relying on other fancy structures - this actually gives us a decent performance already.
      The reason is: remember that we are using a lazy language, if we just need first few
      of a sorted list, we don't get the extra penalty of actually sorting the whole list.
     -}
    sum (snd <$> psq) - prim'sAlgorithm (sortOn snd psq)
  where
    psq :: [] (Edge ,Int)
    psq = foldMap mkBinding weights
      where
        mkBinding (c@(x,y),w) = if x > y then [] else [(c,w)]
    -- should be true. confirming the symmetricity allows us to only store half of
    -- the actual data.
    _isSymmetric = and $ do
      ((x,y),w) <- weights
      case lookup (y,x) weights of
        Just w' -> pure $ w' == w
        Nothing -> pure False
    weights :: [(Edge, Int)]
    weights =
        catMaybes $ zipWith (\c t -> (c,) <$> t) [(r,c) | r <- vertices, c <- vertices] parsed
      where
        parsed :: [Maybe Int]
        parsed = concatMap (fmap parse . T.splitOn ",") . T.lines $ raw
        parse :: T.Text -> Maybe Int
        parse "-" = Nothing
        parse t = Just (read $ T.unpack t)

