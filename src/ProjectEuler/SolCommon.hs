module ProjectEuler.SolCommon
  ( pickInOrder'
  ) where

{-
  This module contains some commonly used functions
  that will eventually be moved into petbox.
  The idea here is to use this module as an intermediate place
  so that we don't need to update petbox that often.
 -}

import Petbox

pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' x = (\(u,v) -> (u,u:v)) <$> pickInOrder x
{-# INLINABLE pickInOrder' #-}

