{-# LANGUAGE DeriveFunctor, DeriveFoldable, OverloadedStrings #-}
module ProjectEuler.Problem128
  ( problem
  ) where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Monoid
import Math.NumberTheory.Primes
import Data.MemoTrie
import Data.Functor
import Data.Foldable

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 128 Unsolved result

{-
  Idea:

  Let's again begin with what we can do:

  - From center up, we have: 1, 2, 8, 20. next few should be: 38, 62.

  - This gives us: https://oeis.org/A077588

    + f(0) = 1
    + f(n) = 3*n*n - 3*n + 2

  - now let's setup a coordinate system so that we can:

    + given a number, compute its coordinate in the system.
    + given a coordinate, compute its 6 neighborhoods.

  - a flat "Cube coordinate" seems like what we want, let's go with that
    
    See https://www.redblobgames.com/grids/hexagons/ regarding "Cube coordinate".

  - note that for the actial representation we'll use Axial Coordinate since
    one axis is redundant.

  - for two cells a & b in cube coordinates, the distance between them is:

    (abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)) / 2

    if we plug in b = (0,0,0) as origin, we can figure out which circle are we in,
    this helps us in finding the mapping between axial coordinate and the number on that grid.

  Update: despite some optimizations, it is still too slow to produce a result,
  we'll need some improvement on the algorithm itself to go further.

  we could try to maintain just "the most recent 3 circles (rings, actually)", which
  has sufficient info for computing the middle ring of them, but this still takes linear time
  to do - so unless something is significantly faster, this is as far as I'm willing to go.

 -}

type AxialCoord = (Int, Int) -- coordinate
type AxialDir = (Int, Int)

-- unit directions following tiles' growing direction.
-- TODO: notice that we can generete a infinite list of diffs, from which
-- we can build up <coordinate, tile number> pairs without using any formula.
-- this might be slow but my hope is that this gives us some insights.
unitDirs :: [] AxialDir
unitDirs =
  [ (-1, 1)
  , (0, 1)
  , (1, 0)
  , (1, -1)
  , (0, -1)
  , (-1, 0)
  ]

plus :: AxialDir -> AxialDir -> AxialDir
plus (x,y) (a,b) = (x+a, y+b)

mul :: Int -> AxialDir -> AxialDir
mul c (x,y) = (c*x, c*y)

dist :: AxialCoord -> AxialCoord -> Int
dist (ax, az) (bx, bz) =
    abs (ax - bx) `max` abs (ay - by) `max` abs (az - bz)
  where
    ay = -ax - az
    by = -bx - bz

{-
  If we number the center cell "circle 0",
  and walk our way outwards around,
  "genTiles n" generates "circle n" for us.
 -}
genTiles :: Int -> [(AxialCoord, Int)]
genTiles 0 = [((0,0), 1)]
genTiles n = zip coords $ take (6*n) [vInit..]
  where
    coords = scanl plus (0,-n) dirs
    dirs = concatMap (replicate n) unitDirs
    vInit = 3*n*n - 3*n + 2

genCoords :: Int -> [AxialCoord]
genCoords 0 = [(0,0)]
genCoords n = take (6*n) coords
  where
    coords = scanl plus (0,-n) dirs
    dirs = concatMap (replicate n) unitDirs

data HC a = HC !a !a !a !a !a !a deriving (Functor, Foldable)

hcInd (HC v0 v1 v2 v3 v4 v5) i = case i of
  0 -> v0
  1 -> v1
  2 -> v2
  3 -> v3
  4 -> v4
  5 -> v5
  _ -> error "out of bound"

mkHexCorners :: Int -> HC (AxialCoord, Int)
mkHexCorners 0 = let z = ((0,0), 1) in HC z z z z z z
mkHexCorners n =
  let vInit = 3*n*n - 3*n + 2
  in HC ((0, -n), vInit)
        ((-n,0), vInit+n)
        ((-n,n), vInit+n*2)
        ((0,n), vInit+n*3)
        ((n,0), vInit+n*4)
        ((n,-n), vInit+n*5)

checkAroundHexCorners :: Int -> [Int]
checkAroundHexCorners n =
    maybeToList (computePdGreaterEqual3 tcPrevCoord) <> mapMaybe (computePdGreaterEqual3 . fst) [tc,c1,c2,c3,c4,c5]
  where
    tcPrevCoord = fst tc `plus` (0,1) `plus` (1,0)
    HC tc c1 c2 c3 c4 c5 = mkHexCorners n

    -- zip [(0,-n), (-n,0), (-n,n), (0,n), (n,0), (n, -n)] [vInit, vInit+n ..]

{-
  TODO: Now, if we can have an efficient way of implementing tileNumToCoord and coordToTileNum,
  I suspect that's sufficient to solve the problem.

  coordToTileNum: we can easily figure out the circle of that coord, but its exact location
  is a bit tricky to do.

  - define "anchors" to be 6 angle tiles on that circle
  - if coord in question is one of those anchors, we already know how to produce a result
  - pair anchors in counter-clockwise order, and compute distance between that coord and pairs,
    there must be one pair that the coord sits in between (and with total distance being n),
    we can then figure out the offset therefore compute the tile number.

  tileNumToCoord:

  - find the positive solution n for  3*n*n - 3*n + 2 = <tileNum>,
    take the floor, hopefully this gives us circle number.

  - note: let f t = (3 + sqrt (12*t-15)) / 6 seems to do it.

  - condition around anchors, like what we are planning to do with coordToTileNum.

 -}
tileNumToCoord :: Int -> AxialCoord
tileNumToCoord = undefined

mkPairs op (HC v0 v1 v2 v3 v4 v5) =
  HC (op v0 v1) (op v1 v2) (op v2 v3) (op v3 v4) (op v4 v5) (op v5 v0)

coordToTileNum :: AxialCoord -> Int
coordToTileNum pt = case lookup pt (toList hcs) of
    Just v -> v
    _ ->
      let dists = fmap (dist pt . fst) hcs
          (cornerInd,_):_ =
            filter ((== n) . snd) $ zip [0..] $ toList $ mkPairs (+) dists
          ((_, tileNum0), offset) = (hcInd hcs cornerInd, hcInd dists cornerInd)
      in tileNum0 + offset
  where
    n = dist pt (0,0)
    hcs = mkHexCorners n

isPrimeMemo :: Int -> Maybe ()
isPrimeMemo = memo (void . isPrime)

computePdGreaterEqual3 :: AxialCoord -> Maybe Int
computePdGreaterEqual3 c = case mapMaybe isPrimeMemo diffs of
    (_:_:_:_) ->
      {-
        Since we know that PD(_) <= 3, we can stop at 3 and claim PD(c) = 3,
        this pattern just look for that particular shape and avoids some computation.
        (since we don't really care about what exactly is that prime)
       -}
      Just x
    _ -> Nothing
  where
    x = coordToTileNum c
    ys = fmap (coordToTileNum . plus c) unitDirs
    diffs = (\y -> abs (y - x)) <$> ys

result = and $ zipWith (==) (take 200 answers) fastCompute
  where
    answers :: [Int]
    answers = foldMap (mapMaybe computePdGreaterEqual3 . genCoords) [0..]
    target = 2000

{-

Some outputs:

Ring #0:
  1: [1,2,3,4,5,6] corner
Ring #1:
  2: [1,1,5,6,7,17] corner
  3: [1,1,2,6,7,8] corner
  4: [1,1,3,7,8,9] corner
  5: [1,1,4,8,9,10] corner
  6: [1,1,5,9,10,11] corner
  7: [1,5,6,10,11,12] corner
Ring #2:
  8: [1,6,11,12,13,29] corner
  9: [1,1,6,7,12,13]
  10: [1,1,7,12,13,14] corner
  11: [1,1,7,8,13,14]
  12: [1,1,8,13,14,15] corner
  13: [1,1,8,9,14,15]
  14: [1,1,9,14,15,16] corner
  15: [1,1,9,10,15,16]
  16: [1,1,10,15,16,17] corner
  17: [1,1,10,11,16,17]
  18: [1,1,11,16,17,18] corner
  19: [1,11,12,17,17,18]
Ring #3:
  20: [1,12,17,18,19,41] corner
  21: [1,1,12,13,18,19]
  22: [1,1,12,13,18,19]
  23: [1,1,13,18,19,20] corner
  24: [1,1,13,14,19,20]
  25: [1,1,13,14,19,20]
  26: [1,1,14,19,20,21] corner
  27: [1,1,14,15,20,21]
  28: [1,1,14,15,20,21]
  29: [1,1,15,20,21,22] corner
  30: [1,1,15,16,21,22]
  31: [1,1,15,16,21,22]
  32: [1,1,16,21,22,23] corner
  33: [1,1,16,17,22,23]
  34: [1,1,16,17,22,23]
  35: [1,1,17,22,23,24] corner
  36: [1,1,17,18,23,24]
  37: [1,17,18,23,24,29]

So there are actually lots of repetitive computations on edge cells,
actually, the list of cell number differences only changes in following cells:

 - all corner cells
 - the cell after corner cell
 - the cell prior to the top corner cell.

By "prior" and "after" I meant cell with number -1 or +1 to the cell in question.

This allows us to reduce search space drastically, as we now have only 6*2 + 1 = 13 cells to check for each ring.

In addition, we can observe that all those edge cells come after corner are always consists of two 1s and two pairs
of consecutive numbers, in other words, their PD values are <= 2,
meaning whatever come after the corner cell can never be a candidate.

 -}
_runDebug :: PEM ()
_runDebug =
  forM_ [4..100] $ \ringInd -> do
    logText $ T.pack $ "Ring #" <> show ringInd <> ":"
    logText $ T.pack $ "  " <> show (checkAroundHexCorners ringInd)

fastCompute :: [Int]
fastCompute = [1,2,8,19,20] <> concatMap checkAroundHexCorners [4..]
