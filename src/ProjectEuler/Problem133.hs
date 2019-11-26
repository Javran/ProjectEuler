module ProjectEuler.Problem133
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Modular
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 133 Unsolved result

divisibleSmall :: Integer -> Bool
divisibleSmall p = not . null $ do
  n <- [1 :: Int .. 200]
  guard $ powMod (10 :: Integer) ((10 :: Integer) ^ n) (9 * p) == 1

{-
  The tricky part is that we don't know how to search for those that will never be a factor,
  so instead we search for those that could be a factor, and see how to go from there.

  The following gives: 11,17,41,73,101,137,251,257,271,353
  which leads me to https://oeis.org/A178070, that sounds like what we want.
 -}
_doSearch :: [Integer]
_doSearch = take 10 $ filter divisibleSmall (fmap unPrime primes)

result = ()
