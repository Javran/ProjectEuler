module ProjectEuler.Problem33
  ( problem
  ) where

import Control.Monad
import Data.List
import Data.Ratio

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 33 Solved result

solutions :: [Ratio Int]
solutions = do
    a <- [10..99]
    b <- [a+1..99]
    -- get the digit appears in both
    [c] <- [show a `intersect` show b]
    guard $ c /= '0'
    -- compute the digit after cancelling.
    let a1 = read $ delete c $ show a
        b1 = read $ delete c $ show b
    -- verify the digit cancelling property
    guard $ a1 * b == a * b1
    pure $ a % b

result :: Int
result = denominator $ product solutions
