module ProjectEuler.Problem128
  ( problem
  ) where

import Data.List
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

{-

  Try this out:
  > let xs = "ABCDE"
  > take 40 $ concat $ concatMap ("!" : ) $ iterate (fmap (\ys@(x:_) -> x : ys)) ((:[]) <$> xs)
  "!ABCDE!AABBCCDDEE!AAABBBCCCDDDEEE!AAAABB"

 -}
dCoords :: [] AxialDir
dCoords =
  concat
  . concatMap ([(0,-1)] :)
  . iterate (fmap (\ys@(x:_) -> x:ys))
  . fmap (:[])
  $ unitDirs

coords :: [] AxialCoord
coords = scanl (\(x,y) (dx,dy) -> (x+dx, y+dy)) (0,0) dCoords

pairs = zip coords [1..]

result = show (take 20 pairs) -- not quite right though: should be (0,-2),8
