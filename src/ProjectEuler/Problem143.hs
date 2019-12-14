module ProjectEuler.Problem143
  ( problem
  ) where

import Petbox
import Control.Monad
import Data.List
import Data.Monoid

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

maxSum = 120000

_genTuples = do
  -- assume that p <= q <= r
  r <- [1 :: Int ..maxSum]
  q <- [1..(r-1)]
  let gcdRQ = gcd r q
  Just a <- [exactSquareRoot (r*r + q*q + r*q)]
  p <- filter ((== 1) . gcd gcdRQ) [1.. min (q-1) (maxSum - r - q)]
  Just b <- [exactSquareRoot (p*p + q*q + p*q)]
  Just c <- [exactSquareRoot (p*p + r*r + p*r)]
  pure ((a,b,c),p+q+r)

{-
  Result of running _genTuples.
  This still takes a while to produce (Time elapsed: 106752.3768 ms),
  definitely something what we'll look into after done.
 -}
tupleList :: [] ((Int, Int, Int), Int)
tupleList = [((511,399,455),784),((665,511,616),1029),((2045,1051,1744),2709),((3441,2089,2405),4424),((4459,1235,4056),5239),((5681,1911,4901),6760),((6223,2917,5672),8265),((5624,1591,5439),6845),((12103,11323,11713),20280),((10101,3395,8456),11809),((15523,12728,13545),24037),((17501,9435,12691),21904),((17501,13889,16856),27735),((18296,12735,16219),27001),((19201,9971,13065),22984),((18928,8827,16835),24843),((22699,13480,17689),30291),((19969,9816,17549),26455),((20280,2743,20033),22477),((22979,8029,22496),29325),((24843,5563,22192),27715),((36005,18088,26657),44403),((27815,3913,26353),30192),((28861,7201,27816),34295),((44593,27037,35815),60648),((37539,14744,33271),47045),((50401,40291,44555),77736),((44135,22477,37128),57967),((57715,44099,45219),84064),((46543,28995,46297),69208),((62797,35152,40605),75563),((58359,38779,49064),83215),((58015,35941,47544),80089),((62543,43453,55608),92345),((76265,41021,64161),101816),((76171,51216,75365),115661),((60853,9373,60840),68783),((73253,31155,64232),93217),((84693,46213,65453),109520),((87935,43989,59641),103544),((84280,48883,68647),113643),((68176,17745,60961),78329),((62699,8971,62261),70080),((72751,20039,67415),86256),((79507,26027,68355),93808),((92680,36741,73151),109561),((95095,42123,77843),118408),((96520,16219,88239),104429),((97976,16219,94335),109561),((102085,10867,101503),111048),((113285,6321,112339),118184)]

result1 = length tupleList
  where
    gen (_, x) = takeWhile (<= maxSum) $ iterate (+ x) x

result = _genTuples
