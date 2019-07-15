module ProjectEuler.Problem102
  ( problem
  ) where

import Data.Monoid

import qualified Data.Text as T

import ProjectEuler.GetData


problem :: Problem
problem = pureProblemWithData "p102_triangles.txt" 102 Unsolved compute

type Coord = (Int, Int)

parseCoords :: String -> [Coord]
parseCoords raw = [(a,b),(c,d),(e,f)]
  where
    [a,b,c,d,e,f] = read ("[" <> raw <> "]")

containsTheOrigin :: [Coord] -> Sum Int
containsTheOrigin [_a,_b,_c] = 0 -- TODO
containsTheOrigin _ = error "unexpected input"

compute :: T.Text -> Int
compute raw =
    getSum $ foldMap containsTheOrigin triangles
  where
    triangles =
      fmap parseCoords
      . lines
      . T.unpack
      $ raw
