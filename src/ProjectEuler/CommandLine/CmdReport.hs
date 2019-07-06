{-# LANGUAGE
    OverloadedLabels
  #-}
module ProjectEuler.CommandLine.CmdReport
  ( cmdReport
  ) where

import Control.Monad

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import ProjectEuler.AllProblems
import ProjectEuler.Types
import ProjectEuler.CommandLine.Common

-- Result type for generating final reports
{-
  The final summary line in the report will be looked like:

  Evaluated {# of problems evaluated} problems in {time elapsed}ms.
  <only lines with non-zero values are shown>
  - Accepted: ?
  - Unverified: ?
  - Wrong: ?
  - Crashed: ?
  - Unsolved: ?
  <exit code is non-zero if any "Wrong" or "Crashed" is present>

 -}
data Result
  = Accepted -- executed successfully, output is expected.
  | Unverified -- executed successfully, but we have no expected answer to compare it against.
  | Wrong -- executed successfully, but answer is not expected
  | Crashed -- the solution crashed
  | Pending -- this problem is marked as unsolved therefore its evaluation is skipped.
    deriving (Show, Ord, Eq)

evalProblem :: Problem -> IO (Double, Result)
evalProblem _ =
  -- TODO: handle actual execution
  pure (0, Pending)

cmdReport :: [String] -> IO ()
cmdReport _ = do
  results <- forM (IM.toAscList allProblems) $ \(_,p) -> evalProblem p
  pure ()
