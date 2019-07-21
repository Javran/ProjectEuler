{-# LANGUAGE
    OverloadedStrings
  #-}
module ProjectEuler.Problem107
  ( problem
  ) where

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

-- compute :: T.Text -> ()
compute raw = show . fmap (fmap parse . T.splitOn ",") . T.lines $ raw
  where
    parse :: T.Text -> Maybe Int
    parse "-" = Nothing
    parse t = Just (read $ T.unpack t)

