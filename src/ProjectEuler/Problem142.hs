module ProjectEuler.Problem142
  ( problem
  ) where

import Control.Monad
import Data.List
import Math.NumberTheory.Powers.Squares

import qualified Data.IntMap.Strict as IM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 142 Solved result

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
    we can begin the search.

  After this is solved:
  - maxN can be 1000.
  - parity concerns.

    let p~q denote that p and q has the same parity.

    + u+v ~ u-v, u ~ u^2, we can skip some steps based on this.
    + a^2 + b^2 = 2x => a ~ b, similarly: c ~ d, e ~ f.

 -}

{-
  Silencing this lint.
  I'll call this "reduce duplication" an overengineering.
 -}
{-# ANN module "HLint: ignore Reduce duplication" #-}

doSearch3Squares :: Int -> IM.IntMap ([] [Int])
doSearch3Squares maxN = IM.fromListWith (<>) $ do
  x <- [1..maxN]
  let part0 = x * x
  y <- [1..x-1]
  let part1 = part0 + y * y
  z <- [1..y-1]
  let r = part1 + z * z
  Just _ <- [exactSquareRoot r]
  pure (r, [[z*z,y*y,part0]])

doSearch2Squares :: Int -> IM.IntMap ([] [Int])
doSearch2Squares maxN = IM.filter ((> 2) . length) $ IM.fromListWith (<>) $ do
  x <- [1..maxN]
  let part0 = x * x
  y <- [1..x-1]
  let r = part0 + y * y
  Just _ <- [exactSquareRoot r]
  pure (r, [[y*y,part0]])

result :: Int
result = fst $ head $ sortOn fst $ do
  let maxN = 1000
      results3Sq = doSearch3Squares maxN
      results2Sq = doSearch2Squares maxN
      commonResultPairs =
        IM.intersectionWith (,) results3Sq results2Sq
  (aSq, (sq3Lists, sq2Lists)) <- IM.toList commonResultPairs
  sq3List <- sq3Lists
  sq2List0 <- sq2Lists -- d^2 = b^2 + f^2
  sq2List1 <- sq2Lists -- c^2 = b^2 + e^2
  guard $ null $ intersect sq2List0 sq2List1
  [bSq, eSq, fSq] <- permutations sq3List
  guard $ eSq > fSq -- this makes sure that we have z > 0
  guard $ even (aSq + bSq)
  guard $ even (eSq + fSq)
  dSq <- delete eSq sq2List0
  guard $ dSq == bSq + fSq
  cSq <- delete fSq sq2List1
  guard $ even (cSq + dSq)
  guard $ cSq == bSq + eSq
  let x = (aSq + bSq) `quot` 2
      y = (eSq + fSq) `quot` 2
      z = (cSq - dSq) `quot` 2
  pure (x + y + z, [x,y,z])
