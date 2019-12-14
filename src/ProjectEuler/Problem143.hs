module ProjectEuler.Problem143
  ( problem
  ) where

import Data.List

import ProjectEuler.Types
import Math.NumberTheory.Powers.Squares

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

  TODO: now we have the correct answer, try to get faster.
 -}

maxSum = 120000

genTuples :: [Int]
genTuples = do
  -- assume that p <= q <= r
  r <- [1 :: Int ..maxSum]
  q <- [1..r]
  let gcdRQ = gcd r q
  -- we are at most computing 120000^2 * 3, using Int will not blow up.
  Just _a <- [exactSquareRoot (r*r + q*q + r*q)]
  p <- filter ((== 1) . gcd gcdRQ) [1.. min q (maxSum - r - q)]
  Just _b <- [exactSquareRoot (p*p + q*q + p*q)]
  Just _c <- [exactSquareRoot (p*p + r*r + p*r)]
  pure $ p+q+r

result = sum $ nub $ concatMap dup genTuples
  where
    dup x = takeWhile (<= maxSum) $ iterate (+ x) x
