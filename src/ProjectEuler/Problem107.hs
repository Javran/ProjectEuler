{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  #-}
module ProjectEuler.Problem107
  ( problem
  ) where

import Data.Function
import Data.Maybe
import Data.List
import Control.Monad.ST
import Control.Monad

import qualified Data.Text as T
import qualified Data.PSQueue as PSQ
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
problem = pureProblemWithData "p107_network.txt" 107 Unsolved compute

type Coord = (Int, Int)

vertices :: [Int]
vertices = [0..39]

prim'sAlgorithm :: PSQ.PSQ Coord Int -> Int
prim'sAlgorithm initPsq = runST $ do
  -- `clsSz ! v` stores equivalence class size of a vertex
  -- note that the data is only valid when the vertex is the representative.
  clsSz <- MVec.replicate 40 (1 :: Int)
  uf <- MVec.unsafeNew 40
  -- initialize union-find-set
  forM_ vertices $ \v -> do
    s <- UF.fresh v
    MVec.write uf v s
  (fix $ \loop psq result -> do
           sz0 <- MVec.read clsSz 0
           if sz0 == 40
             then pure result
             else
               case PSQ.minView psq of
                 Nothing -> error "no more edge to try."
                 Just ((x,y) PSQ.:-> w, psq') -> do
                   px <- MVec.read uf x
                   py <- MVec.read uf y
                   connected <- UF.equivalent px py
                   if connected
                     then loop psq' result
                     else do
                       UF.union' px py $ \_ _ -> do
                         szX <- MVec.read clsSz x
                         szY <- MVec.read clsSz y
                         MVec.write clsSz y (szX + szY)
                         pure y
                       loop psq' (result + w)

    ) initPsq (0 :: Int)

compute :: T.Text -> Int
compute raw = prim'sAlgorithm psq
  where
    psq :: PSQ.PSQ Coord Int
    psq = PSQ.fromList $ foldMap mkBinding weights
      where
        mkBinding (c@(x,y),w) = if x > y then [] else [c PSQ.:-> w]
    -- should be true. confirming the symmetricity allows us to only store half of
    -- the actual data.
    _isSymmetric = and $ do
      ((x,y),w) <- weights
      case lookup (y,x) weights of
        Just w' -> pure $ w' == w
        Nothing -> pure False
    weights :: [(Coord, Int)]
    weights =
        catMaybes $ zipWith (\c t -> (c,) <$> t) [(r,c) | r <- vertices, c <- vertices] parsed
      where
        parsed :: [Maybe Int]
        parsed = concatMap (fmap parse . T.splitOn ",") . T.lines $ raw
        parse :: T.Text -> Maybe Int
        parse "-" = Nothing
        parse t = Just (read $ T.unpack t)

