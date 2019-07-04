module ProjectEuler.Problem66
  ( problem
  ) where

import Data.Ratio

import ProjectEuler.Types
import ProjectEuler.Problem64 (gen)

problem :: Problem
problem = pureProblem 66 Unsolved result

approx :: [Integer] -> [Rational -> Rational]
approx = tail . scanl (\acc i t -> acc (1 / (i % 1 + t))) id

{-
  Some leads:
  - https://en.wikipedia.org/wiki/Pell's_equation
  - Problem64
 -}
result :: [Double]
result =
  take 10 . map (fromRational . (1 /) . ($ 0))
  . approx . map (toInteger . fst) $ gen 7
