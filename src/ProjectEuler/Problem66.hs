module ProjectEuler.Problem66
  ( problem
  )
where

import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Ratio
import Math.NumberTheory.Roots
import ProjectEuler.Problem64 (gen)
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 66 Solved result

{-

  Given an non-empty list of continued fraction sequence a0, a1, a2...,

  we want to build a list of expressions contains one hole in it
  (denoted below by `?`)

  - first element is `a0 + ?`
  - second element is `a0 + 1 / (a1 + ?)`
  - third element is `a0 + 1 / (a1 + 1 / (a2 + ?))`
  - ... and so on ...

  as we can see this allows building up the list in an iterative fashion
  while we can plug `0` into `?` anytime we want to get the fraction.

  Admittedly this would be quadratic because no actual computation is shared,
  but I feel this should be good enough for this particular problem.

 -}
approx :: [Integer] -> [Rational -> Rational]
approx (x : xs) = scanl (\acc i t -> acc (1 / (i % 1 + t))) (x % 1 +) xs
approx [] = error "unreachable"

solve :: Int -> (Integer, Integer)
solve d = head . mapMaybe isSolution $ approxs
  where
    isSolution r =
      if x * x - toInteger d * y * y == 1
        then Just (x, y)
        else Nothing
      where
        x = numerator r
        y = denominator r
    approxs = map ($ 0) . approx . map (toInteger . fst) $ gen d

{-
  This problem is exactly https://en.wikipedia.org/wiki/Pell's_equation,
  in which solutions can be obtained through consecutive approximations
  through continued fractions of the square root of number D.

  As Problem64 finds the loop for a square root of certain number,
  we can reuse some of those logics to give us a (infinite) list
  of consecutive approximations, on which we can try each of those fractions
  to find the first solution of it, which is supposed to be the minimal solution.

  Continuing from this point is straightforward: find the maximum, and
  it corresponding D.
 -}
result :: Int
result = fst . maximumBy (comparing snd) $ do
  d <- filter (not . isSquare) [2 .. 1000]
  let (x, _) = solve d
  pure (d, x)
