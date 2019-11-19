module ProjectEuler.Problem128
  ( problem
  ) where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Monoid
import Math.NumberTheory.Primes.Testing

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

plus (x,y) (a,b) = (x+a, y+b)

dCoords :: [] AxialDir
dCoords =
  -- TODO: not the best method in the world, but this does give us correct results.
  concatMap (\xs -> let ys = concat xs in init ys <> [plus (last ys) (0,-1)])
  . iterate (fmap (\ys@(x:_) -> x:ys))
  . fmap (:[])
  $ unitDirs

pairs :: [] (AxialCoord, Int)
pairs = ((0,0), 1) : zip coords [2..]
  where
    coords :: [] AxialCoord
    coords = scanl plus (0,-1) dCoords

-- n >= 1 for this to work.
mkTiles :: Int -> M.Map AxialCoord Int
mkTiles n = M.fromList $ take count pairs
  where
    count = 3*n*n - 3*n + 2

computePDs :: Int -> [(Int, Int)]
computePDs n = foldMap go coords
  where
    go cur = do
      let x = tiles M.! cur
      let tileNums = mapMaybe ((tiles M.!?) . plus cur) unitDirs
      ys@[_,_,_,_,_,_] <- pure tileNums
      let diffs = (\y -> abs (y - x)) <$> ys
      pure (x, getSum $ foldMap (\v -> if isPrime (fromIntegral v) then 1 else 0) diffs)

    coords = M.keys tiles
    tiles = mkTiles n

result = (!! 9) $ filter ((== 3) . snd ) $  sortOn fst $ computePDs 20
