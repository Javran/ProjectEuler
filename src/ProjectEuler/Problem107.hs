{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  #-}
module ProjectEuler.Problem107
  ( problem
  ) where

import Data.Maybe
import qualified Data.Text as T

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

-- compute :: T.Text -> ()
compute raw = isSymmetric
  where
    -- should be true. confirming the symmetricity allows us to only store half of
    -- the actual data.
    isSymmetric = and $ do
      ((x,y),w) <- weights
      case lookup (y,x) weights of
        Just w' -> pure $ w' == w
        Nothing -> pure False
    weights :: [(Coord, Int)]
    weights =
        catMaybes $ zipWith (\c t -> (c,) <$> t) [(r,c) | r <- [0..39], c <- [0..39]] parsed
      where
        parsed :: [Maybe Int]
        parsed = concatMap (fmap parse . T.splitOn ",") . T.lines $ raw
        parse :: T.Text -> Maybe Int
        parse "-" = Nothing
        parse t = Just (read $ T.unpack t)

