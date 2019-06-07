module ProjectEuler.Types
  ( ProblemStatus(..)
  , Problem(..)
  ) where

data ProblemStatus = Solved | Unsolved

data Problem
  = Problem
  { problemId :: Int
  , problemStatus :: ProblemStatus
  , problemRun :: [String] -> IO ()
  }
