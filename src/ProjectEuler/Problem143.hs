module ProjectEuler.Problem143
  ( problem
  ) where

import ProjectEuler.Types

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

 -}

result = ()
