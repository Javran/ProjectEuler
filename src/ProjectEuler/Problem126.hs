module ProjectEuler.Problem126
  ( problem
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List.Ordered as LO

import ProjectEuler.Types

import Petbox

problem :: Problem
problem = pureProblem 126 Unsolved result

{-
  Idea: appears tricky because of the complexity involved
  with face-covering cubes - we can't simply count from
  all directions because of intersections.

  Well, let's figure out those numbers with the stupid way.

  (Note: there used to be more code for doing the actual simulation
  of this layer-by-layer cuboid covering, but they got removed to
  tidy up. See commit history of this file.)

  Now some interesting findings:

  > cuboidCovering 1 2 3
  [22,46,78,118,166,222,286,358,438,526,622,726,838,958,1086,1222,1366,1518,1678,1846,...]
  This sequence seems to be 4 n^2 + 12 n + 6 (n = 1, 2, ...)

  > cuboidCovering 11 1 1
  [46,98,158,226,302,386,478,578,686,802,926,1058,1198,1346,1502,1666,1838,2018,2206,2402,...]
  This sequence seems to be 4 n^2 + 40 n + 2 (n = 1, 2, ...)

  It does make sense that the growth is quadratic, given that if we do this covering
  for infinite number of steps, we'll end up getting a sphere, whose surface area is 4 pi n^2.

  Alright, we've figured out the exact formula, let's worry about how to get to the answer now.

 -}

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
cuboidCovering :: Int -> Int -> Int -> [Int]
cuboidCovering x y z = l <$> [1..]
  where
    a = x*y + y*z + x*z
    b = x+y+z
    l n = 4*n*n + 4*(b-3)*n + (2*a-4*b+8)

runLengthEncoding :: Eq a => [a] -> [(Int, a)]
runLengthEncoding (x:xs) = (1+ length ys, x) : runLengthEncoding zs
  where
    (ys,zs) = span (== x) xs
runLengthEncoding [] = []

result =
    {-
      TODO: Now the following does give (10, 154), as desired, but this is too slow
      and maxSize remains a guesswork.
     -}
    firstSuchThat ((== 1000) . fst) $ runLengthEncoding (foldr LO.merge [] covSeqs)
  where
    maxSize = 400
    covSeqs = [ cuboidCovering x y z | x <- [1..maxSize], y <- [x..maxSize], z <-[y..maxSize] ]
