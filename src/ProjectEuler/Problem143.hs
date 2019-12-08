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
 -}

-- http://oeis.org/A089025 looks promising for only primitives. (not exactly the same sequence it seems)
-- http://oeis.org/A061281: this one seems to have some that doesn't qualify, but if we can find a way to
-- test on numbers, this could work.
result = LOrdered.nub $ fmap snd $ sortOn snd $ do
  a <- [1..2000]
  b <- [1..a]
  let gab = gcd a b
      cMax = integerSquareRoot (a*a + b*b + a*b) - 1
  -- lower bound encodes a < b + c => c > a - b
  -- upper bound encodes  a*a + b*b + a*b > c * c (i.e. largest corner < 2 pi / 3)
  c <- [a-b+1 .. b]
  guard $ gcd gab c == 1
  guard $ a*a + b*b + a*b > c * c && b*b + c*c + b*c > a*a && a*a + c*c + a*c > b*b
  -- (i.e. largest corner < 2 pi / 3)
  -- guard $ a < b + c -- no need of testing (b < a + c && c < a + b) because a >= b >= c >= 1
  let t :: Int
      t = a^!4+b^!4+c^!4-(a^!2-b^!2)^!2-(b^!2-c^!2)^!2-(a^!2-c^!2)^!2
  Just tR <- [exactSquareRoot (3*t)]
  let t1 = a*a + b*b + c*c + tR
  (lSq, 0) <- [t1 `quotRem` 2]
  Just l <- [exactSquareRoot lSq]
  guard $ even t1
  -- l = sqrt((a^2+b^2+c^2 + sqrt(3)*sqrt(t))/2)
  -- l = sqrt((a^2+b^2+c^2 + tR)/2)
  pure ((c,b,a),l)
