{-# LANGUAGE LambdaCase #-}
module ProjectEuler.Problem143
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Powers.Squares
import Petbox

import qualified Data.DList as DL
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 143 Unsolved result

{-
  Idea:

  Reading some part of https://en.wikipedia.org/wiki/Fermat_point gives me the idea that,
  in our case where all angles are less than 2pi / 3, angle ATB = BTC = CTA = 2 pi / 3.

  From this insight, we can:
  - start from a "barebone" that divides 2 pi evenly into 3 parts with segment p, q and r
    (by doing so, we can bound on p+q+r more easily)
  - connect their other sides to form the triangle, and check whether a,b,c are all integers.
    note that by applying cosine rules, we have:

    + a^2 = r^2 + q^2 + r*q
    + b^2 = p^2 + q^2 + p*q
    + c^2 = p^2 + r^2 + p*r

  A brute force search is slow but is fast enough to get to an answer.

  My original approach is to enumerate 0 < a <= b <= c and figure out p,q,r that way.
  But this approach has several drawbacks:

  - There is no clear way to bound on p+q+r.

  - I wasn't aware that p, q, r are all required to be integer, not just p,q,r.

    You probably have noticed that:

    fmap (/7) [399,455,511,784] == [57.0,65.0,73.0,112.0]

    Unaware of this constraint on p,q,r leads me to many spurious tuples.

  - I've tried many ways to reduce the amount of search space,
    you can find many of my writeups in older version of this file,
    but with algorithm established, optimization can only get you so far.

  However there is one thing I do want to keep here so we can appreciate it:

  p + q + r = sqrt((a^2+b^2+c^2 + sqrt(3)*sqrt((a^2 + b^2 + c^2)^2 - 2*(a^4 + b^4 + c^4)))/2)

  The efficient method is described in the overview after solving the problem.

 -}

maxSum :: Int
maxSum = 120000

{-
  This is the "overview" method:

  - i = 2 m n + n^2
  - j = m^2 - n^2
  - k = m^2 + m n + n^2

  With m > n, gcd(m,n) = 1 and (m - n) `mod` 3 /= 0.
  Note that here we know k > i and k > j, but both i > j and i < j are possible.
  to make things a bit easier, (i,j,k) :: PrimTuple assumes that i <= j <= k.

 -}

type PrimTuple = (Int, Int, Int) -- i <= j <= k

{-
  Analysis on the example given in problem's description:

  a = 399, b = 455, c = 511
  p = 195, q = 264, r = 325

  p,q,a forms a triangle: (195,264,399) = 3 * (65,88,133)
  q,r,c forms a triangle: (264,325,511) (primitive)
  r,p,b forms a triangle: (325,195,511) (primitive)

 -}

{-
  Build up primitive tuples indexed by two shorter sides of the triangle.
 -}
prims :: IM.IntMap [PrimTuple]
prims =
    -- this filter rules out values that are singleton lists.
    -- since we want to pick 3 triangles that can join together to form a larger one,
    -- those singletons are never useful.
    IM.filter (\case
                  -- no case for empty list.
                  -- due to the fact that this is a dictionary, there's no need of that.
                  [_] -> False
                  _ -> True)
    . IM.map DL.toList
    . IM.fromListWith (<>)
    $ concatMap
        (\t@(i,j,_) -> let d = DL.singleton t in [(i,d),(j,d)])
        primTuples
  where
    {-
      TODO: update doc.
      p + q + r <= maxSum

      Here we can relax this constraint to make it a bit easier:

      p + q + 1 <= maxSum

      p + q + 1
      = 2 m n + n^2 + m^2 - n^2
      = 2 m n + m^2 + 1 <= 2m^2 + m^2 + 1 == 3m^2 + 1 <= maxSum

      3m^2 < maxSum

      Well, let's just say m <= integerSquareRoot (maxSum / 3),
      once we have the triple, fine-grain checks can be applied.
     -}
    -- TODO: this bound is wrong.
    maxM = integerSquareRoot' maxSum
    primTuples :: [] PrimTuple
    primTuples = do
      m <- [1..maxM]
      n <- [1..m-1]
      guard $ (m-n) `rem` 3 /= 0
      guard $ gcd m n == 1
      let i = 2*m*n + n*n
          j = m*m - n*n
          k = m*m + m*n + n*n
          (i',j') = if i <= j then (i,j) else (j,i)
          maxScale = maxSum `quot` (i+j)
      [(i'*s,j'*s,k*s) | s <- [1..maxScale] ]

doSearch :: [] (Int, Int, Int)
doSearch = do
  -- here we assume that p <= q <= r, and pick p in the first step.
  (p, tsPre) <- IM.toAscList prims
  -- now that p is the shortest,
  -- we are only interested in those greater than p
  let ts = filter (\(u,v,_) -> u >= p && v >= p) tsPre
  -- pick two tuples from the list
  ((_,y0,_),ts0) <- pickInOrder ts
  ((_,y1,_),_) <- pickInOrder ts0
  -- we now have two "other sides",
  -- let's assign q,r so that q <= r.
  let (q,r) = if y0 <= y1 then (y0, y1) else (y1, y0)
  guard $ p+q+r <= maxSum
  -- now we have q and r, what we need to do is to simple look it up.
  Just vs <- [prims IM.!? q]
  -- just simply need to check whether it's possible,
  -- no need of actually getting that value.
  guard $ any (\(x,y,_z) -> (x,y) == (q,r)) vs
  pure (p,q,r)

result :: Int
result =
  IS.foldr' (+) 0
  . IS.fromList
  . fmap (\(p,q,r) -> p+q+r)
  $ doSearch
