module ProjectEuler.Problem102
  ( problem
  ) where

import Data.Monoid

import qualified Data.Text as T

import ProjectEuler.GetData

problem :: Problem
problem = pureProblemWithData "p102_triangles.txt" 102 Solved compute

{-
  Idea: https://en.wikipedia.org/wiki/Cross_product

  For all 3 vertices,
  we compute cross product for every directed edge and vector that points to origin,
  if origin always stay at one side of each directed edge, we know origin is inside.

  To see whether they are all at one side, we just need to example non-zero
  cross products and see if they are all of the same sign.
 -}

type Coord = (Int, Int)
type Vec = (Int, Int)

parseCoords :: String -> [Coord]
parseCoords raw = [(a,b),(c,d),(e,f)]
  where
    [a,b,c,d,e,f] = read ("[" <> raw <> "]")

cDiff :: Coord -> Coord -> Vec
cDiff (x,y) (u,v) = (x-u,y-v)

cross :: Vec -> Vec -> Int
cross (a0,a1) (b0,b1) = a0*b1 - a1*b0

containsTheOrigin :: [Coord] -> Sum Int
containsTheOrigin [ca,cb,cc] =
    if all (> 0) allNonZeroCross || all (< 0) allNonZeroCross
      then 1
      else 0
  where
    o = (0,0)

    vAB = cDiff cb ca
    vBC = cDiff cc cb
    vCA = cDiff ca cc

    vAO = cDiff o ca
    vBO = cDiff o cb
    vCO = cDiff o cc

    crossA = cross vAB vAO
    crossB = cross vBC vBO
    crossC = cross vCA vCO

    allNonZeroCross = filter (/= 0) [crossA, crossB, crossC]

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
