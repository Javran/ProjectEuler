module ProjectEuler.Problem206
  ( problem
  ) where

import Data.Int
import Math.NumberTheory.Powers.Squares
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 206 Solved result

searchSpaceDiv10 :: [Int64]
searchSpaceDiv10 = do
    -- further, since N / 100 ends in '9',
    -- the last digit of n must be either 3 or 7
    -- so we can further reduce the search range
    x <-[lBound .. uBound]
    [x*10+3, x*10+7]
  where
    -- let's say this perfect square number is n*n = N
    -- the last unknown digit of N must be '0',
    -- because the last digit of N is '0',
    -- to make N a perfect square, n must be divisible by 10
    -- therefore the search space is dramatically reduced
    lBound, uBound :: Int64
    lBound = integerSquareRoot' 10203040506070809 `div` 10
    uBound = integerSquareRoot' 19293949596979899 `div` 10

-- check the pattern
valid :: Int64 -> Bool
valid xDiv10
  | [9,_,8,_,7,_,6,_,5,_,4,_,3,_,2,_,1] <-
      intToDigitsRev (xDiv10 * xDiv10) = True
  | otherwise = False

result :: Int64
result = (*10) . firstSuchThat valid $ searchSpaceDiv10

