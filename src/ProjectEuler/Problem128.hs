{-# LANGUAGE DeriveFunctor #-}
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

mkTiles :: Int -> M.Map AxialCoord Int
mkTiles n =
  M.unions $
    uncurry M.singleton (head (genTiles n)) : fmap (M.fromList . genTiles) [0..n-1]

mkHexCorners :: Int -> [] (AxialCoord, Int)
mkHexCorners 0 = replicate 6 ((0,0), 1)
mkHexCorners n =
  let vInit = 3*n*n - 3*n + 2
  in zip [(0,-n), (-n,0), (-n,n), (0,n), (n,0), (n, -n)] [vInit, vInit+n ..]

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

coordToTileNum :: AxialCoord -> Int
coordToTileNum pt = case lookup pt hcs of
    Just v -> v
    _ ->
      let dists = fmap (dist pt . fst) hcs
          (cornerInd,_):_ =
            filter ((== n) . snd)  $ zip [0..] $ zipWith (+) dists (tail (cycle dists))
          ((_, tileNum0), offset) = (hcs !! cornerInd, dists !! cornerInd)
      in tileNum0 + offset
  where
    n = dist pt (0,0)
    hcs = mkHexCorners n

computePDs :: Int -> [(Int, Int)]
computePDs n = foldMap go coords
  where
    go cur = do
      let x = tiles M.! cur
      let tileNums = mapMaybe ((tiles M.!?) . plus cur) unitDirs
      ys@[_,_,_,_,_,_] <- pure tileNums
      let diffs = (\y -> abs (y - x)) <$> ys
      pure (x, getSum $ foldMap (\v -> case isPrime v of Just _ -> 1; _ -> 0) diffs)

    coords = M.keys tiles
    tiles = mkTiles n

isPrimeMemo :: Int -> Maybe ()
isPrimeMemo = memo (void . isPrime)

computePdGreaterEqual3 :: AxialCoord -> Bool
computePdGreaterEqual3 c = case mapMaybe isPrimeMemo diffs of
    (_:_:_:_) -> True
    _ -> False
  where
    x = coordToTileNum c
    ys = fmap (coordToTileNum . plus c) unitDirs
    diffs = (\y -> abs (y - x)) <$> ys

result =
    (!! (target - 1))
      $ fmap fst $ filter snd
      $ concatMap (fmap ((\x -> (coordToTileNum x, computePdGreaterEqual3 x)) . fst) . genTiles) [0..]
  where
    target = 100
