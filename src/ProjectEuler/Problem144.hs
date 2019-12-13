module ProjectEuler.Problem144
  ( problem
  ) where

import Data.List

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = Problem 144 Unsolved run

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
-- nextPoint :: Point -> Point -> Point
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

run = mapM_ (logText . fmt) $ take 50 $ unfoldr (Just . go) (point0, point1)
  where
    fmt (x,y) = T.pack $ show x <> " " <> show y
    go (pt0, pt1) = (pt1, (pt1, pt2))
      where
        pt2 = nextPoint pt0 pt1
