module ProjectEuler.SolCommon
  ( pickInOrder'
  ) where

{-
  This module contains some commonly used functions
  that will eventually be moved into petbox.
  The idea here is to use this module as an intermediate place
  so that we don't need to update petbox that often.
 -}

import Data.List

{-
  like pickInOrder but the element being picked is not removed from the list,
  therefore has the effect of allowing a previously picked element to be picked again.
 -}
pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' = fmap (\(x:xs) -> (x,x:xs)) . init . tails
{-# INLINABLE pickInOrder' #-}

