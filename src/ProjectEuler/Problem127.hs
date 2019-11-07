module ProjectEuler.Problem127
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Primes
import Math.NumberTheory.Euclidean
import Data.Monoid

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 127 Unsolved result

rad :: Int -> Int
rad n = product $ unPrime . fst <$> factorise n

{-
  No idea at first, as always. But there are few things that might come in handy:

  - note that a + b = c, so we only need to search for two numbers and the third one
    can be derived from that.

  - given that a, b, c are pair-wise coprimes, therefore
    rad(a * b * c) = rad(a) * rad(b) * rad(c), by definition (no shared prime factor).

  - c = a + b, if c < 1000, we know a + b < 1000

  Update: now the example (c < 1000) given by the problem is working,
  but it is too slow by simply plugging in 120000, we need to do something else.

 -}

searchAbcHits maxN = do
  b <- [2..maxN]
  a <- filter (coprime b) . takeWhile (< (maxN - b)) $ [1..b-1]
  let c = a + b
  guard $ coprime b c && coprime a c && rad a * rad b * rad c < c
  pure (a,b,c)

result = getSum $ foldMap (\(_,_,c) -> Sum c) xs
  where
    xs = searchAbcHits 1000
