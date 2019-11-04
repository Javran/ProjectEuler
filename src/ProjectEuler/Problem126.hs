{-# LANGUAGE DeriveFunctor #-}
module ProjectEuler.Problem126
  ( problem
  ) where

import Control.Monad
import Data.List

import qualified Data.Set as S
import qualified Control.Foldl as L

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 126 Unsolved result

{-
  Idea: appears tricky because of the complexity involved
  with face-covering cubes - we can't simply count from
  all directions because of intersections.

  Well, let's figure out those numbers with the stupid way.

  Now some interesting findings:

  > cuboidCovering 1 2 3
  22,46,78,118,166,222,286,358,438,526,622,726,838,958,1086,1222,1366,1518,1678,1846
  This sequence seems to be 4 n^2 + 12 n + 6 (n = 1, 2, ...)

  > cuboidCovering 11 1 1
  46,98,158,226,302,386,478,578,686,802,926,1058,1198,1346,1502,1666,1838,2018,2206,2402
  This sequence seems to be 4 n^2 + 40 n + 2 (n = 1, 2, ...)

  It does make sense that the growth is quadratic, given that if we do this covering
  for infinite number of steps, we'll end up getting a sphere, whose surface area is 4 pi n^2.

 -}

data Coord a
  = Coord
  { coordX :: a
  , coordY :: a
  , coordZ :: a
  }
  deriving (Ord, Eq, Functor, Show)

type Shape = S.Set (Coord Int) -- Shape are blocks represented by set of their coordinates

neighbors :: Coord Int -> [Coord Int]
neighbors (Coord x y z) =
  [ Coord (x-1) y z , Coord (x+1) y z
  , Coord x (y-1) z , Coord x (y+1) z
  , Coord x y (z-1) , Coord x y (z+1)
  ]

coverShape :: Shape -> Shape
coverShape s = S.fromList $ do
    x <- [minX-1 .. maxX+1]
    y <- [minY-1 .. maxY+1]
    z <- [minZ-1 .. maxZ+1]
    let c = Coord x y z
    -- near existing shape but itself is not one part of it.
    guard $ any (`S.member` s) (neighbors c)
    guard $ S.notMember c s
    pure c
  where
    cs = S.toList s
    getMinMax getter = (minV, maxV)
      where
        (Just minV, Just maxV) =
          L.fold ((,) <$> L.minimum <*> L.maximum) . fmap getter $ cs
    -- Admittedly this is not the most efficient way of getting mins and maxs,
    -- but let's not worry about efficiency for now.
    (minX, maxX) = getMinMax coordX
    (minY, maxY) = getMinMax coordY
    (minZ, maxZ) = getMinMax coordZ

cuboidCovering :: Int -> Int -> Int -> [Int]
cuboidCovering x y z = unfoldr next initShape
  where
    next s = Just (S.size incr, S.union s incr)
      where
        incr = coverShape s
    initShape = S.fromList $
      Coord <$> [1..x] <*> [1..y] <*> [1..z]

result =
  [ take 20 (cuboidCovering 1 2 3)
  , take 3 (cuboidCovering 5 1 1)
  , take 3 (cuboidCovering 5 3 1)
  , take 3 (cuboidCovering 7 2 1)
  , take 20 (cuboidCovering 11 1 1)
  ]
