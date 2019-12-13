module ProjectEuler.Problem144
  ( problem
  ) where

import Data.List
import Petbox

import qualified Data.Text as T

import ProjectEuler.Types


problem :: Problem
problem = pureProblem 144 Unsolved result

{-
  Idea:

  Let's first work out some formulas that might become necessary.

  - Given a point (x_0, y_0) on the ellipse, we should be able to find the tangent line,
    from where we can figure out the reflection.

    y_0 = -4 x_0^2 / y_0 + b => b = (y_0^2 + 4x_0^2) / y_0 = 100 / y_0

    Therefore the tangent line is: 4x_0 x + y_0 y - 100 = 0

    And the slope for the normal line = -1 / m = -1 / ( -4 x_0 / y_0 ) = y_0 / (4 x_0)

    And the normal line is: y_0 x - 4 x_0 y + 3 x_0 y_0 = 0

  - For two given points A, B on the ellipse, we should be able to figure out the next point,
    this involves:

    + connect A and B to form a line.
    + figure out how line AB reflects when hitting tangent line at B
    + on the reflected line, find point C on the ellipse.
      (we should just need the resulting slope to figure out the line,
      this is because we have assumed that B is on the ellipse.)

 -}

type Point = (Double, Double)
type V2 = (Double, Double)

point0, point1 :: Point
point0 = (0, 10.1)
point1 = (1.4, -9.6)

toUnit :: V2 -> V2
toUnit (x,y) = (x / d,y / d)
  where
    d = sqrt $ x*x + y*y

diff :: Point -> Point -> V2
diff (a0,b0) (a1,b1) = (a0-a1,b0-b1)

cross :: V2 -> V2 -> Double
cross (a0,b0) (a1,b1) = a0 * b1 - a1 * b0

distSq :: Point -> Point -> Double
distSq (xA,yA) (xB,yB) = (xA-xB)^(2 :: Int) + (yA-yB)^(2 :: Int)

{-
  Requires that pointB to be on the ellipse.

  TODO:
  - figure out unit vector for A -> B (call this u)
  - figure out unit vector for tangent at B (call this t)
  - u x t gives an angle (could be negative), if we rotate t by that angle,
    the resulting vector has the slope of the reflected line.
 -}
nextPoint :: Point -> Point -> Point
nextPoint pointA pointB@(xB,yB) =
    if distSq pt0 pointB < distSq pt1 pointB
      then pt1
      else pt0
  where
    vecU = toUnit $ diff pointB pointA
    vecT@(dxT,dyT) = toUnit (yB, -4 * xB) -- convert from slope
    sineTheta = cross vecU vecT -- the direction of reflection is vector t rotated by theta.
    cosineTheta = sqrt (1 - sineTheta * sineTheta)
    vecU'@(dxU',dyU') = (dxT * cosineTheta - dyT * sineTheta, dxT * sineTheta + dyT * cosineTheta)
    -- it should be the case that cross vecU vecT == cross vecT vecU'
    -- m is slope of u'
    m = dyU' / dxU'
    b = yB - m * xB
    delta = 400 * m * m - 16 * b * b + 1600
    x0 = (-2*m*b + sqrt delta) / (2 * (4 + m*m))
    x1 = (-2*m*b - sqrt delta) / (2 * (4 + m*m))
    pt0 = (x0, m*x0 + b)
    pt1 = (x1, m*x1 + b)
    {-
      yB = m * xB + b => b = yB - m * xB

      - 4x^2 + y^2 = 100
      - y = m * x + b

      discriminant = 400 m^2 - 16 b^2 + 1600

     -}

{-
  Here are 50 points generated for using gnuplot for a visual verification:

  1.4 -9.6
  -3.9905976193616177 -6.024991498863841
  0.32458287808384445 9.978906945202928
  0.4164904357818666 -9.965246753974995
  -4.473443497221033 4.466901958705094
  1.1323378646973856 9.740187053680758
  5.3650666227919835e-2 -9.999424304631402
  -1.834442373413963 9.302649338467692
  3.288249509749743 7.5332370629446075
  -0.22649147776392195 -9.989735053643876
  -0.566604327517025 9.935584438982744
  4.9043104270523665 -1.9470379915198812
  -0.852425522663563 -9.853602534771078
  -0.11872610922242588 9.997180424697534
  2.3747408946139372 -8.800137654252506
  -2.615512387847177 -8.522697917682631
  0.14655157781557387 9.995703604057049
  0.7610819050196274 -9.883471927182606
  -4.981530796736325 -0.8587221230818711
  0.6368968968992175 9.91854068756491
  0.19334674607421926 -9.992520610092834
  -3.0161672800006487 7.975646666961492
  2.03349686329844 9.135620505900057
  -7.838654327826237e-2 -9.99877103444876
  -1.0138087400312523 9.79228100876119
  4.677230354958941 3.5346944460027387
  -0.47086766253242757 -9.95555797419336
  -0.28348101128361813 9.98391476651151
  3.7179664302226856 -6.6863220454109715
  -1.5583638907057538 -9.501894965562276
  1.6527799669369146e-2 9.999945366218375
  1.3410299891504862 -9.633615846233237
  -4.095888446893905 -5.73539809624442
  0.34202582033842277 9.976576234003694
  0.3962776516617539 -9.968543328449442
  -4.382458899620835 4.814168253451109
  1.1829624198601882 9.716089730585763
  4.400059833845979e-2 -9.999612781972282
  -1.7597800494284364 9.360165421109537
  3.398381275631299 7.335122277218992
  -0.24044091059075776 -9.988430941547183
  -0.5405461155003715 9.941390224112006
  4.857676431972964 -2.3689488658511912
  -0.8914989030577212 -9.839762132459683
  -0.10807016478782465 9.997663895027225
  2.2832435680603007 -8.896470942775291
  -2.71553654688531 -8.39663295911646
  0.158114718947752 9.99499869647857
  0.7272445339229124 -9.893657642728346
  -4.995629667644142 -0.4180154243741523

 -}
result = fst $ firstSuchThat (\(_,(x,y)) -> x >= -0.01 && x <= 0.01 && y > 0) $
    zip [0 :: Int ..] (unfoldr (Just . go) (point0, point1))
  where
    fmt (x,y) = T.pack $ show x <> " " <> show y
    go (pt0, pt1) = (pt1, (pt1, pt2))
      where
        pt2 = nextPoint pt0 pt1
