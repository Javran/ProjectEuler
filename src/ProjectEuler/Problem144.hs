module ProjectEuler.Problem144
  ( problem
  ) where

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

{-
  Requires that pointB to be on the ellipse.

  TODO:
  - figure out unit vector for A -> B (call this u)
  - figure out unit vector for tangent at B (call this t)
  - u x t gives an angle (could be negative), if we rotate t by that angle,
    the resulting vector has the slope of the reflected line.
 -}
nextPoint :: Point -> Point -> Point
nextPoint pointA pointB@(xB,yB) = undefined
  where
    vecU = toUnit $ diff pointB pointA
    vecT@(dxT,dyT) = toUnit (1, -4 * xB / yB) -- convert from slope
    sineTheta = cross vecU vecT -- the direction of reflection is vector t rotated by theta.
    cosineTheta = sqrt (1 - sineTheta * sineTheta)
    vecU'@(dxU',dyU') = (dxT * cosineTheta - dyT * sineTheta, dxT * sineTheta + dyT * cosineTheta)
    slopeU' = dyU' / dxU'

result = 4 * x * x + y * y
  where
    (x,y) = point1
