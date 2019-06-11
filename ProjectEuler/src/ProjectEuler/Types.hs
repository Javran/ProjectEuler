module ProjectEuler.Types
  ( ProblemStatus(..)
  , Problem(..)
  , pureProblem
  ) where

data ProblemStatus = Solved | Unsolved

data Problem
  = Problem
  { problemId :: Int
  , problemStatus :: ProblemStatus
  , problemRun :: [String] -> IO ()
  }

pureProblem :: Show r => Int -> ProblemStatus -> r -> Problem
pureProblem pId pSt result =
  Problem pId pSt (const $ print result)
