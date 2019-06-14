module ProjectEuler.Types
  ( ProblemStatus(..)
  , Problem(..)
  , pureProblem
  ) where

data ProblemStatus = Solved | Unsolved

{-
  problemRun performs some arbitrary action,
  and perhaps give some outputs through stdout to
  help deriving final answers to the problem.

  (TODO) We might need some printing facility
  instead of outputing to stdout.

  It it also intentional that this action does not
  take any arguments - if we want to setup tests
  or run it against some dynamic inputs,
  we can just export module internals.
 -}
data Problem
  = Problem
  { problemId :: Int
  , problemStatus :: ProblemStatus
  , problemRun :: IO ()
  }

pureProblem :: Show r => Int -> ProblemStatus -> r -> Problem
pureProblem pId pSt result =
  Problem pId pSt (print result)
