module ProjectEuler.Problem127
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Primes
import Math.NumberTheory.Euclidean
import Data.Monoid

import qualified Data.Vector as V

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 127 Unsolved result

maxN :: Int
maxN = 120000

radVec :: V.Vector Int
radVec = V.fromListN maxN $ undefined : fmap radImpl [1..]
  where
    radImpl :: Int -> Int
    radImpl = getProduct . foldMap (Product . unPrime . fst) . factorise

rad :: Int -> Int
rad = (radVec V.!)

{-
  No idea at first, as always. But there are few things that might come in handy:

  - note that a + b = c, so we only need to search for two numbers and the third one
    can be derived from that.

  - given that a, b, c are pair-wise coprimes, therefore
    rad(a * b * c) = rad(a) * rad(b) * rad(c), by definition (no shared prime factor).

  - c = a + b, if c < 1000, we know a + b < 1000

  Update: now the example (c < 1000) given by the problem is working,
  but it is too slow to simply plugging in 120000, we need to do something else.

  Update: brute forced the answer: 18407904, but the problem is designed in
  a way such that we can do this much more faster, going to investigate on that.

 -}

searchAbcHits = do
  b <- [2..maxN]
  a <- filter (coprime b) [1..min (b-1) (maxN-b-1)]
  let rab = rad a * rad b
      c = a + b
  guard $ coprime b c && coprime a c && rab * rad c < c
  pure (a,b,c)

result = getSum $ foldMap (\(_,_,c) -> Sum c) searchAbcHits
