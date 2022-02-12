module ProjectEuler.Problem85
  ( problem
  ) where

import Control.Monad
import Data.List
import Data.Ord
import Math.NumberTheory.Roots
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 85 Solved result

{-
  consider the function "countRect 1 n" with stands for
  rectangle grids with m rows and n cols

  when m = 1, there are
  * n   rectangles with size 1x1
  * n-1 rectangles with size 1x2
  ...
  * 1   rectangle  with size 1xn

  therefore countRect 1 n = n + (n-1) + ... + 1 = n*(n+1) / 2

  now consider "countRect m n", in which we have m rows instead of one
  we can extend "countRect 1 n" to "countRect m n" by mapping from each original
  rectangle in "countRect 1 n" into m*(m+1)/2 rectangles in "countRect m n"

  therefore the formula is n*(n+1)*m*(m+1) / 4

  note that the formula is symmetry since "countRect m n = countRect m n" for any valid choice
  of m and n, we can safely assume that n >= m to reduce search space when applicable.


countRect :: Int -> Int -> Int
countRect m n = halve (n * (n+1)) * halve (m * (m+1))

  we don't actually need this function. the problem asks for the nearest solution
  for n*(n+1)*m*(m+1) / 4 = 2,000,000 => n*(n+1)*m*(m+1) = 8,000,000 where n >= m

  approximately we want (n*m)^2 to be as close to 8,000,000 as possible,
  taking "nearest solution" into account, we also want to exceed 8,000,000
  a little bit in case some number greater turns out to be closer
-}

result :: Int
result = fst $ minimumBy (comparing snd) solutions
  where
    targetMul4 = 8000000 :: Int
    -- "100" is a magic number
    prodMN = 100 + integerSquareRoot targetMul4
    solutions = do
        n <- [1..prodMN]
        m <- [1..n]
        let prods = n * m * (n+1) * (m+1)
            diff = abs (prods-targetMul4)
        guard $ diff <= 1000
        return (m*n, diff)

