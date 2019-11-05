{-# LANGUAGE
    DeriveFunctor
  #-}
module ProjectEuler.Problem126
  ( problem
  ) where

import Control.Monad
import Data.List

import qualified Data.Set as S

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

data Dim3 a
  = Dim3
  { dimX :: !a
  , dimY :: !a
  , dimZ :: !a
  }
  deriving (Ord, Eq, Functor, Show)

type Shape = S.Set (Dim3 Int) -- Shape are blocks represented by set of their coordinates

neighbors :: Dim3 Int -> [Dim3 Int]
neighbors (Dim3 x y z) =
  [ Dim3 (x-1) y z , Dim3 (x+1) y z
  , Dim3 x (y-1) z , Dim3 x (y+1) z
  , Dim3 x y (z-1) , Dim3 x y (z+1)
  ]

coverShape :: Dim3 (Int, Int) -> Shape -> Shape
coverShape (Dim3 (minX,maxX) (minY,maxY) (minZ,maxZ)) s = S.fromList $ do
    c <- Dim3 <$> [minX .. maxX] <*> [minY .. maxY] <*> [minZ .. maxZ]
    -- near existing shape but itself is not one part of it.
    guard $ any (`S.member` s) (neighbors c)
    guard $ S.notMember c s
    pure c

cuboidCovering :: Int -> Int -> Int -> [Int]
cuboidCovering x y z = unfoldr next (initShape, initBound)
  where
    next (s, curBound) = Just (S.size incr, (S.union s incr, nxtBnd))
      where
        nxtBnd = fmap (\(lo,hi) -> (lo-1,hi+1)) curBound
        incr = coverShape nxtBnd s
    initBound = Dim3 (1,x) (1,y) (1,z)
    initShape = S.fromList $
      Dim3 <$> [1..x] <*> [1..y] <*> [1..z]

cuboidCoveringSpeedup1 :: Int -> Int -> Int -> [Int]
cuboidCoveringSpeedup1 x y z = seq2
  where
    a:b:c:_ = cuboidCovering x y z
    dba = b - a
    dcb = c - b
    dd = dcb - dba
    seq0 = repeat dd
    seq1 = dba : zipWith (+) seq1 seq0
    seq2 = a : zipWith (+) seq2 seq1

{-
  For a cuboid measuring X x Y x Z, generate an infinite list of number of cubes
  necessary for covering current shape.

  Let's say that this cuboid "grows" outwards by 1 each time, and we have
  the following observations:

  - The size of 6 faces of the cuboid stays the same.
    Because this is a layer-by-layer procedure, edge & corner can never outgrow.

    Let A = x*y + y*z + x*z, each layer will always need the same amount of cubes to
    cover its all faces, which is:

    2*A


  - The length of all 12 edges also stays the same.
    In fact edges do become longer, but let's say count those extra growth
    on edges as growth on corners.

    However, despite that edge length never grows, we do need more cubes
    as we have more layers:

                                #
                          #     X#
                ->  #  -> X# -> XX#
    L1(nothing)     L2    L3    L4

    - L1: first layer, no need to cover any edge
    - L2: second layer, need to cover one edge
    - L3: third layer, needs 2 set of edges to cover previous one.
    - and so forth.

    Therefore, let B = x + y + z, for the n-th layer (n >= 1),
    the number of cubes needed for covering that layer is:

    4*B*(n-1)

  - We have 8 corners, all are covered in the same way:

    - First layer got no corner to cover
    - Second layer need 1 cube on each of the 8 corners to cover.
    - Third layer need 3 (1+2) cube to cover exposing faces of previous cover, on each of 8 corners.
    - Fourth layer, 6 (1+2+3) cubes on each of 8 corners.
    - Fifth layer, 10 (1+2+3+4) cubes on each of 8 corners.

    You've seen the pattern, for the n-th layer (n >= 1),
    the number of cubes needed for covering that layer is:

    8 * ((n-1) * (n-2) / 2) => 4*(n-1)*(n-2)

  Put it all together, for the n-th layer (n >= 1), the number of cubes needed in total is:

  2*A + 4*B*(n-1) + 4 * (n-1) * (n-2)
  => 2*A + 4*(n-1)*(n+(B-2))
  => 4*n*n + 4*(B-3)*n + (2*A-4*B+8)

 -}
cuboidCoveringSpeedup2 :: Int -> Int -> Int -> [Int]
cuboidCoveringSpeedup2 x y z = l <$> [1..]
  where
    a = x*y + y*z + x*z
    b = x+y+z
    l n = 4*n*n + 4*(b-3)*n + (2*a-4*b+8)

{-
  Some observations from printing out those coefficients:

  - we can bascially expect s1, s2, s3 to all be even numbers,
    therefore we can simply use integral division on b and c.
  - a always seems to be 4.
 -}
cuboidCoveringCoeff :: Int -> Int -> Int -> (Int, Int, Int)
cuboidCoveringCoeff x y z = (a, b, c)
  where
    s1:s2:s3:_ = cuboidCovering x y z
    a = (s1 - 2 * s2 + s3) `quot` 2
    -- b and c seems to always be Ints.
    b = ((-5) * s1 + 8 * s2 - 3 * s3) `quot` 2
    c = 3 * s1 - 3 * s2 + s3

{-
  This is to verify the correctness of cuboidCoveringFast in
  a native way: compute and compare the result of both.
  This does take a long time to run, but since any False
  value will immediately terminate the computation,
  we know this holds at least for some small cases.

  Confirmed results:
  - _verifyCover 20 16 is True
  - _verifyCover 5 20 is True
  - _verifyCover 20 20 is True
 -}
_verifyCover :: Int -> Int -> Bool
_verifyCover limit mx = and
  [ take limit result0 == take limit result1
  | x <- [1..mx]
  , y <- [x..mx]
  , z <- [y..mx]
  , let result0 = cuboidCovering x y z
  , let result1 = cuboidCoveringSpeedup2 x y z
  ]

result = _verifyCover 20 20
