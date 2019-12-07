module ProjectEuler.Problem142
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Powers.Squares

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 142 Unsolved result

{-
  We can start manipulating some equations
  and see if any interesting property comes out of it.

  let:
  - x + y = a^2
  - x - y = b^2
  - x + z = c^2
  - x - z = d^2
  - y + z = e^2
  - y - z = f^2

  therefore we have:
  - 2x = a^2 + b^2 = c^2 + d^2
  - 2y = a^2 - b^2 = e^2 + f^2
  - 2z = c^2 - d^2 = e^2 - f^2
  - a^2 + b^2 = c^2 + d^2
  - a^2 = b^2 + e^2 + f^2 = e^2 + d^2 = c^2 + f^2
  - d^2 = b^2 + f^2
  - c^2 = b^2 + e^2

  Some observation:
  - a is the largest in {a,b,c,d,e,f},
    which can be sum of 2 or 3 perfect squares.
  - if we assume e^2 = c^2, then c^2 = b^2 + e^2,
    which would imply b = 0, therefore x = y.
    therefore e /= c.
    if we assume e^2 = f^2, we'll have z = 0, therefore e /= f.
    therefore e^2 + d^2 = c^2 + f^2 cannot be the same pair of perfect square.
  - some simple reasoning allows us to say b /= e, e /= f, b /= f,
    so a^2 is sum of three different perfect squares, I guess this is where
    we can begin the search

 -}

doSearch3Squares :: Int -> IM.IntMap ([] [Int])
doSearch3Squares maxN = IM.fromListWith (<>) $ do
  x <- [1..maxN]
  let part0 = x * x
  y <- [1..x-1]
  let part1 = part0 + y * y
  z <- [1..y-1]
  let r = part1 + z * z
  Just _ <- [exactSquareRoot r]
  pure (r, [[z,y,x]])

doSearch2Squares :: Int -> IM.IntMap ([] [Int])
doSearch2Squares maxN = IM.filter (\xs -> length xs >= 2) $ IM.fromListWith (<>) $ do
  x <- [1..maxN]
  let part0 = x * x
  y <- [1..x-1]
  let r = part0 + y * y
  Just _ <- [exactSquareRoot r]
  pure (r, [[y,x]])

result = do
  let results3Sq = doSearch3Squares 1000
      results2Sq = doSearch2Squares 1000
      commons =
        IS.toAscList $ IS.intersection (IM.keysSet results3Sq) (IM.keysSet results2Sq)
  aSq <- commons
  sq3List <- results3Sq IM.! aSq -- a^2 = b^2 + e^2 + f^2
  sq2List0 <- results2Sq IM.! aSq -- d^2 = b^2 + f^2
  sq2List1 <- results2Sq IM.! aSq -- c^2 = b^2 + e^2
  guard $ sq2List0 /= sq2List1
  guard $ any (`elem` sq2List0) sq3List
  guard $ any (`elem` sq2List1) sq3List
  pure (aSq, sq3List, sq2List0, sq2List1)
