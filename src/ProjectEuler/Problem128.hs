{-# LANGUAGE DeriveFunctor, DeriveFoldable, OverloadedStrings #-}
module ProjectEuler.Problem128
  ( problem
  ) where

import Data.Foldable
import Data.Functor
import Data.Maybe
import Math.NumberTheory.Primes

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 128 Solved result

{-
  Idea:

  Let's again begin with what we can do:

  - From center up, we have: 1, 2, 8, 20. next few should be: 38, 62.

  - This gives us: https://oeis.org/A077588

    + f(0) = 1
    + f(n) = 3*n*n - 3*n + 2

  - now let's setup a coordinate system so that when we are
    given a coordinate, the tile numbers of its 6 neighborhoods can be easily computed.

  - Here we'll go with "Axial Coordinate" (flat version) as demonstrated in:
    
    https://www.redblobgames.com/grids/hexagons/

  - to figure out ring id for a given cell, we can simply compute the distance between it and (0,0)
    (note: the center cell is ring #0, and then we have ring #1 around the center one,
    and ring #2, #3, ... growing outwards)

  - It is too slow to go from tile #1, tile #2, ... all the way up, we got to find some optimizations.

  - Now if we print out 6 differences for each cell for each ring, we can easily spot some patterns:

    (1) we notice that all edge cells *not* next to any corner cells are simply repeating all 6 differences from
        previous cell, this does make sense, say if we are moving from B to B+1:

         A     A+1
       D   D+1     D+2
         B     B+1
       E   E+1     E+2
         C     C+1

      ----- direction of growth ----->

      It is now obvious that this move does not change any of those 6 differences,
      so actually, for each ring, we simply need to test those around corner cells.

    (2) further, we can observe that for the edge cells *not* next to any corner cells,
        there are always two 1s and two pairs of consective numbers
        (so there are exactly 2 even number within those 4 numbers), e.g.:

      - 15: [1,1,9,10,15,16]
      - 17: [1,1,10,11,16,17]
      - 21: [1,1,12,13,18,19]
      - 25: [1,1,13,14,19,20]
      - 31: [1,1,15,16,21,22]

      This allows us to conclude that the PD values for those numbers are always <= 2,
      therefore we can skip them all.

    (3) by excluding those edges described in (2), we have only few cells to check for each ring:

      - all corner cells
      - the cell after corner cell, but it is unnecessary to check this one, due to (2):
        since those edge cells following a corner cell are always a repetition of previous cell,
        that means the cell right next to the corner cell is also one of those cells whose PD value is <= 2.
      - the cell prior to the top corner cell.

      By "prior" and "after" I meant cell with number -1 or +1 to the cell in question.

  - With those discoveries above, we managed to get the correct answer - more optimization could follow.

 -}

type AxialCoord = (Int, Int) -- coordinate
type AxialDir = (Int, Int)

-- unit directions following tiles' growing direction.
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

dist :: AxialCoord -> AxialCoord -> Int
dist (ax, az) (bx, bz) =
    abs (ax - bx) `max` abs (ay - by) `max` abs (az - bz)
  where
    ay = -ax - az
    by = -bx - bz

{-
  If we number the center cell / ring as "ring #0",
  and walk our way outwards around,
  "genCoords n" generates coordinates for "ring #n" for us.
 -}
genCoords :: Int -> [AxialCoord]
genCoords 0 = [(0,0)]
genCoords n = take (6*n) coords
  where
    coords = scanl plus (0,-n) dirs
    dirs = concatMap (replicate n) unitDirs

data HC a = HC !a !a !a !a !a !a deriving (Functor, Foldable)

hcInd :: HC a -> Int -> a
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
    maybeToList (computePdGreaterEqual3 tcPrevCoord)
    <> mapMaybe (computePdGreaterEqual3 . fst) [tc,c1,c2,c3,c4,c5]
  where
    tcPrevCoord = fst tc `plus` (0,1) `plus` (1,0)
    HC tc c1 c2 c3 c4 c5 = mkHexCorners n

mkPairs :: (t -> t -> a) -> HC t -> HC a
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

computePdGreaterEqual3 :: AxialCoord -> Maybe Int
computePdGreaterEqual3 c = case mapMaybe (void . isPrime) diffs of
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

fastCompute :: [Int]
fastCompute =
  -- here "init" is used to exclude last generated coordinate because
  -- it will be handled by ring #4's top corner.
  mapMaybe computePdGreaterEqual3 (init $ concatMap genCoords [0..3])
  <> concatMap checkAroundHexCorners [4..]

result :: Int
result = fastCompute !! (2000 - 1)
