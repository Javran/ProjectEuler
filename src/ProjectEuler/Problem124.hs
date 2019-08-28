module ProjectEuler.Problem124
  ( problem
  ) where

import Math.NumberTheory.Primes.Factorisation
import Control.Monad

import qualified Data.List.Ordered as LOrdered
import qualified Petbox
import qualified Data.IntMap.Strict as IM
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 124 Solved result

{-
  Idea: instead of enumerating all numbers in range
  and produce their radicals, I plan to:

  - generate all radicals within range first
  - then for each radical, generate numbers of that radical
    (so we basically need the radical number and the set of primes)
  - note that in the sorted results, numbers from same radical
    is always groupped together, we can take advantage of this property
    to skip through elements.

  Update: generating them take more time, experimenting factorization on all of them.

 -}

maxN :: Int
maxN = 100000

-- for this particular problem, these are all primes that we need to consider.
primes :: [Int]
primes = takeWhile (< maxN) Petbox.primes

-- NOTE: this might not be as practical as I thought - generating them takes more time.
_genRadicals curProduct curList candidates = do
  (c, candidates') <- Petbox.pickInOrder candidates
  let curProduct' = c * curProduct
  guard $ curProduct' < maxN
  [(curProduct', c:curList)] <> _genRadicals curProduct' (c:curList) candidates'

calcRadical :: Int -> Int
calcRadical x = product (fromIntegral . fst <$> factorise (fromIntegral x))

{- TODO: cleanup -}

result =
    (!! 9999)
    . foldMap snd
    . IM.toAscList
    $ IM.fromListWith LOrdered.union (toPair <$> [1..maxN])
  where
    toPair :: Int -> (Int, [Int])
    toPair x = (calcRadical x, [x])
