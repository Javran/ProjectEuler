{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectEuler.Problem132
  ( problem
  )
where

import Control.Monad
import Data.Mod.Word
import Data.Proxy
import GHC.TypeLits
import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 132 Solved result

{-
  Apparently this doesn't sound right to me to
  actually carry out the division - let's see if we can
  go through primes and do divisibility tests.

  A repunit is R(k) = (10^k-1) / 9, and we want to find a list of
  primes (starting from smallest) divisible by it.

  Yes the following can be done in modular arithmetic,
  but we don't want that 9 to be absolved
  (as in scaling property a === b => a*k === b*k (mod p), which we are trying to avoid.)

  (10^(10^9) - 1) / 9 = k * p + 0 (k is an integer)
  > 10^(10^9) - 1 = k * p * 9
  > 10^(10^9) = 1 + k * 9*p

 -}

result :: Int
result =
  sum
    . take 40
    $ do
      p <- primes
      case someNatVal (fromIntegral (9 * p)) of
        Just (SomeNat (Proxy :: Proxy md)) -> do
          let a = 10 :: Data.Mod.Word.Mod md
          p <$ guard (a ^% (1_000_000_000 :: Int) == 1)
        Nothing -> error "unreachable"
