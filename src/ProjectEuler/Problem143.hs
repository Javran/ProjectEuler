module ProjectEuler.Problem143
  ( problem
  ) where

import Petbox
import Control.Monad
import Data.List

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types
import Math.NumberTheory.Powers.Squares

problem :: Problem
problem = pureProblem 143 Unsolved result

{-
  First let's work out a formula for that length.
  The example in problem (let's compute length of AN):

  > let a = 399; b = 455; c = 511
  > let t = (a*a + c*c - b*b) / (2*a*c) -- cos value of B
  > let t' = sqrt (1 - t*t) -- sin value of B
  > let cosTheta = t / 2 - (sqrt 3 / 2) * t'
  > sqrt (a*a + c*c - 2*a*c*cosTheta)
  784.0

  This whole stuff can be simplified to:
  (Thanks to https://www.symbolab.com/)

  > let xs@[a,b,c] = [399,455,511]
  > let sqz t = sum . fmap (^t)
  > (/ 2) $ sqz 2 xs + sqrt 3 * sqrt (2 * sqz 2 [a*c,a*b,c*b] - sqz 4 xs)
  614656.0
  > sqrt it
  784.0

  Or for a more copy-and-paste friendly version:

  sqrt((a^2+b^2+c^2 + sqrt(3) * sqrt(-a^4-b^4-c^4 +2*a^2*b^2+2*a^2*c^2+2*b^2*c^2)) / 2)
  > sqrt((a^2+b^2+c^2 + sqrt(3)*sqrt(a^4+b^4+c^4-(a^2-b^2)^2-(b^2-c^2)^2-(a^2-c^2)^2))/2)
  > sqrt((a^2+b^2+c^2 + sqrt(3)*sqrt((a^2 + b^2 + c^2)^2 - 2*(a^4 + b^4 + c^4)))/2)
  - We can probably begin with searching primitive pairs gcd(a,b,c) = 1
    and scale them up to get a bunch of solutions.

  - Now the problem is to find a strategy for searching edges...
  - Another tricky problem: the bound is on p + q + r, I'm not sure how should I put bound on
    triangles.

  Cosine rule: for 3 edges of triangle a,b,c (corners are A,B,C):

  - a^2 + b^2 - 2*a*b*cosC == c^2 => cosC = (a^2 + b^2 - c^2) / (2*a*b)
  - C > 2pi/3 => cosC > -1/2 (since 0 < C < pi)
    => (a^2 + b^2 - c^2) / (2*a*b) > -1/2
    => (a^2 + b^2 - c^2) / (  a*b) > -1
    =>  a^2 + b^2 - c^2 > -a*b
    =>  a^2 + b^2 + a*b > c^2

  Alternative approach:
  Reading some part of https://en.wikipedia.org/wiki/Fermat_point gives me the idea that,
  in our case where all angles are less than 2pi / 3, angle ATB = BTC = CTA = 2 pi / 3.

  From this insight, we can:
  - start from a "barebone" that divides 2 pi evenly into 3 parts with segment p, q and r
    (by doing so, we can bound on p+q+r more easily)
  - connect their other sides to form the triangle, and check whether a,b,c are all integers.

 -}

-- http://oeis.org/A089025 looks promising for only primitives. (not exactly the same sequence it seems)
-- http://oeis.org/A061281: this one seems to have some that doesn't qualify, but if we can find a way to
-- test on numbers, this could work.
{-
  Now I'm wondering how to make this more efficient:
  - skip some small values of b, not sure how to bound those yet.
  - generate integer triples? if we can make this construction to only generate
    triangles that does not have a >= 2pi / 3 corner, this will be ideal to
    perform some filtering.
 -}

_result = LOrdered.nub $ fmap snd $ sortOn snd $ do
  -- search a,b,c: 0 < a <= b <= c
  c <- [1..2000]
  -- say a need to have at least one value to take between c-b+1 and b
  -- b - (c-b+1) + 1 > 0
  -- > b - c + b - 1 + 1 = 2b - c > 0 => b > c/2 >= floor(c/2)
  b <- [quot c 2+1 ..c]
  let gcb = gcd c b
      aMax = integerSquareRoot (c*c + b*b + c*b) - 1
  -- lower bound encodes c < b + a => a > c - b
  -- upper bound encodes  c*c + b*b + c*b > a*a (i.e. largest corner < 2 pi / 3)
  let aMin = ((integerSquareRoot (4*c*c - 3*b*b) - b) `quot` 2) + 1
  a <-
    -- dropWhile (\a' -> a'*a' + a'*b <= c*c - b*b)
    filter (\a' -> gcd gcb a' == 1) [max (c-b+1) aMin .. min aMax b]
  -- guard $ gcd gcb a == 1
  -- guard $ a*a + b*b + a*b > c*c
  {-
    a*a + a*b + b*b - c*c > 0

    A=1
    B=b
    C=b*b - c*c

    DELTA = B^2 - 4AC = b^2 - 4*1*(b*b - c*c) = b^2 - 4*b*b + 4*c*c = 4*c*c - 3*b*b
    If we solve a => a = (-B + sqrt DELTA) / 2 (dropping negative solution as a > 0)
    a > (-B + sqrt DELTA) / 2 >= (-B + intSqrt DELTA) / 2 >= floor((-B + intSqrt DELTA) / 2)

   -}
  -- guard $ b*b + c*c + b*c > a*a

  -- We want to verify that: a*a + c*c + a*c > b*b <=> a*a + a*c > b*b - c*c
  -- well, b < c, therefore b*b - c*c < 0, while a*a + a*c > 0, no need of checking.
  -- guard $ a*a + c*c + a*c > b*b

  -- (i.e. largest corner < 2 pi / 3)
  -- guard $ a < b + c -- no need of testing (b < a + c && c < a + b) because c >= b >= a >= 1
  let t :: Int
      -- t = a^!4+b^!4+c^!4-(a^!2-b^!2)^!2-(b^!2-c^!2)^!2-(a^!2-c^!2)^!2
      -- t = 2*a*a*b*b - b*b*b*b + 2*b*b*c*c - c*c*c*c + 2*c*c*a*a - a*a*a*a
      t = sqSum^!2 - 2*(a^!4 + b^!4 + c^!4)
      sqSum = a*a + b*b + c*c
  Just tR <- [exactSquareRoot (3*t)]
  let t1 = sqSum + tR
  (lSq, 0) <- [t1 `quotRem` 2]
  Just l <- [exactSquareRoot lSq]
  -- l = sqrt((a^2+b^2+c^2 + sqrt(3)*sqrt(t))/2)
  -- l = sqrt((a^2+b^2+c^2 + tR)/2)
  pure ((a,b,c),l)

{-
  Brute force but with p,q,r rather than a,b,c - for now this looks promising.
 -}
result = do
  -- assume that p <= q <= r
  r <- [1 :: Int ..120000]
  q <- [1..r]
  let gcdRQ = gcd r q
  Just a <- [exactSquareRoot (r*r + q*q + r*q)]
  p <- filter ((== 1) . gcd gcdRQ) [1..q]
  Just b <- [exactSquareRoot (p*p + q*q + p*q)]
  Just c <- [exactSquareRoot (p*p + r*r + p*r)]
  pure ((a,b,c),p+q+r)
