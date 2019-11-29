module ProjectEuler.Problem135
  ( problem
  ) where

import Data.Monoid
import Control.Monad
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.ArithmeticFunctions
import Data.Word
import Control.Monad.ST

import qualified Data.List.Match
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 135 Solved result

{-
  Let's get some symbol pushing going:

  (m+d)^2 - m^2 - (m-d)^2 = n > 0.

  w.l.o.g.: m > d > 0.
  this also implies that m > 1, we don't have a value for d otherwise.

  (m+d)^2 - m^2 - (m-d)^2
  = (m+d+m-d)(m+d-m+d) - m^2
  = 4 m d - m^2
  = (4 d - m) * m = n > 0

  so this gives us another constraint:

  4 d - m > 0 => m < 4 d,

  so for a fixed value n, m can be one of its divisor.

  d = (n + m^2) / (4 * m) must be an integer.

  If we further plug in d < m:

  (n + m^2) / 4 * m < m
  >  n < 3 * m^2

  This gives us a tighter range on m.
  Also note that since d can be expressed directly using only m and n,
  we only need to search for m and verify that d is valid.
  Let's try this idea out.

 -}

findSameDiffs :: Int -> [] (Int, Int)
findSameDiffs n = do
  let lo = integerSquareRoot' (n `quot` 3)
      (_, ds) = IS.split lo $ divisorsSmall n
  {-
    m > sqrt(n / 3) >= floor( sqrt(n / 3) ),
    therefore we can safely start with lo+1.
   -}
  m <- IS.toList ds
  let numer = n + m * m
      denom = 4 * m
  (d, 0) <- pure $ numer `quotRem` denom
  pure (m, d)

countSameDiffs :: Int -> VU.Vector Word16
countSameDiffs maxN = runST $ do
  vec <- VUM.replicate (maxN+1) (0 :: Word16)
  forM_ [2 .. maxN] $ \m ->
    forM_ (takeWhile (<= maxN) [m, m+m..]) $ \n -> do
      let numer = n + m * m
          denom = 4 * m
      case numer `quotRem` denom of
        (d, 0) | d < m -> VUM.modify vec succ n
        _ -> pure ()
  VU.unsafeFreeze vec

exactly10 :: [a] -> Bool
exactly10 = Data.List.Match.equalLength (replicate 10 ())

{- TODO: yup, we can do faster by avoiding divisorsSmall repeatly and count in the other direction -}

result :: Int
result =
  getSum $
    foldMap
      (\n -> if n == 10 then 1 else 0)
      $ VU.toList $ countSameDiffs (1000000-1)

