module ProjectEuler.Problem144
  ( problem
  ) where

import Data.Ratio

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

type Point = (Rational, Rational)

point0, point1 :: Point
point0 = (0, 101 % 10)
point1 = (14 % 10, -96 % 10)

{-
  Requires that pointB to be on the ellipse.
 -}
nextPoint :: Point -> Point -> Point
nextPoint pointA pointB = undefined

result = 4 * x * x + y * y
  where
    (x,y) = point1
