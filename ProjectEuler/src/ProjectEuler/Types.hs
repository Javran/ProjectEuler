{-# LANGUAGE RankNTypes #-}
module ProjectEuler.Types
  ( ProblemStatus(..)
  , Problem(..)
  ) where

data ProblemStatus = Solved | Unsolved

class Problem a where
  getStatus :: forall p. p a -> ProblemStatus
  getStatus _ = Unsolved

  run :: forall p. p a -> [String] -> IO ()
  run _ _ = pure ()
