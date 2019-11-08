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


{-
  No idea at first, as always. But there are few things that might come in handy:

  - note that a + b = c, so we only need to search for two numbers and the third one
    can be derived from that.

  - given that a, b, c are pair-wise coprimes, therefore
    rad(a * b * c) = rad(a) * rad(b) * rad(c), by definition (no shared prime factor).

  - c = a + b, if c < 1000, we know a + b < 1000

  - c = a + b and a < b, therefore c < 2*b

  - b > 2, because:

    + when b = 2, (a,b,c) = (1,2,3), in which rad(a*b*c) < c does not hold.
    + b > 1, otherwise there is no value for a to take.

  - rad(c) >= 2, this is simply because c > b > 2.

  - rad(c) >= 2 && c < 2*b && rad(a)*rad(b)*rad(c) < c therefore:

    rad(a)*rad(b)*rad(c) < c < 2 * b <= rad(c) * b
    => rad(a) * rad(b) < b

    TODO: there are some rooms of improvement from this fact:
    - precompute  rad(_), create a mapping from rad(x) to list of x.
    - if we start with searching b, we can constrain a to a very limited search space.
    - note that gcd(rad(a), rad(b)) = 1 iff. gcd(a,b) = 1, therefore
      we gain more speed by this groupping on rad(_), as there are less coprime tests to do.

  Update: now the example (c < 1000) given by the problem is working,
  but it is too slow to simply plugging in 120000, we need to do something else.

  Update: brute forced the answer: 18407904, but the problem is designed in
  a way such that we can do this much more faster, going to investigate on that.

 -}

maxN :: Int
maxN = 120000

radVec :: V.Vector Int
radVec =
    V.fromListN (maxN+1) $
      {-
        laziness in action: the actual computation only happen when that position in vector
        is accessed for the first time.
       -}
      undefined : fmap radImpl [1..]
  where
    radImpl :: Int -> Int
    radImpl = getProduct . foldMap (Product . unPrime . fst) . factorise

rad :: Int -> Int
rad = (radVec V.!) -- requires that 0 < input <= maxN

searchAbcHits :: [(Int, Int, Int)]
searchAbcHits = do
  b <- [3..maxN]
  let rb = rad b
  -- c < maxN => a + b < maxN => a <= maxN - b - 1
  a <- filter (\a -> coprime a b && rad a * rb < b) [1..min (b-1) (maxN-b-1)]
  let rab = rad a * rb
      c = a + b
  {-
    At this point we know a and b does not share any factor,
    therefore c = a + b is already coprime to both a and b,
    there is no need to check this fact.
   -}
  guard $ rab * rad c < c
  pure (a,b,c)

result :: Int
result = getSum $ foldMap (\(_,_,c) -> Sum c) searchAbcHits
