module ProjectEuler.Problem127
  ( problem
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Monoid
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Primes

import qualified Data.DList as DL
import qualified Data.IntMap as IM
import qualified Data.Vector as V

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 127 Solved result


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

revRadMap :: [] (Int, [Int])
revRadMap =
  (fmap . second) DL.toList
  . IM.toAscList
    -- append it the other way to keep the values sorted.
  . IM.fromListWith (flip (<>))
  . fmap (\x -> (rad x, DL.singleton x))
  $ [1..maxN]

searchAbcHits :: [(Int, Int, Int)]
searchAbcHits = do
  (radB, bs) <- revRadMap
  b <- bs
  -- we are searching radA where radA * radB < b
  (radA, as) <- takeWhile (\(radA', _) -> radA' * radB < b) revRadMap
  guard $ coprime radB radA
  let rab = radA * radB
  -- a < b && a + b < maxN => a < min(b,maxN-b)
  a <- takeWhile (< min b (maxN-b)) as
  let c = a + b
  guard $ rab * rad c < c
  pure (a,b,c)

result :: Int
result = getSum $ foldMap (\(_,_,c) -> Sum c) searchAbcHits
