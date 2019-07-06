{-# LANGUAGE
    NamedFieldPuns
  , TupleSections
  #-}
module ProjectEuler.CommandLine.CmdReport
  ( cmdReport
  ) where

import Control.Monad
import Data.Maybe
import Numeric.Sum
import System.Exit
import Text.Printf
import Control.Exception

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import ProjectEuler.AllProblems
import ProjectEuler.GetData
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

-- TODO: haskeline for better output?
evalProblem :: Problem -> IO (Double, Result)
evalProblem p@Problem {problemId, problemStatus}
  | problemStatus == Unsolved = do
      let t = 0
      fmt t Pending
      pure (t, Pending)
  | otherwise = do
      (t, execResult) <- runProblem p
      case execResult of
        Left e -> do
          let r = Crashed
          fmt t r
          putStrLn $ "  Error message: " <> displayException e
          pure (t, r)
        Right outs -> case getExpectedAnswers problemId of
          Nothing -> do
            let r = Unverified
            fmt t r
            pure (t, r)
          Just expects -> if expects == outs
            then do
              let r = Accepted
              fmt t r
              pure (t, r)
            else do
              let r = Wrong
              fmt t r
              putStrLn $ "  Expected: " <> show expects
              putStrLn $ "  Actual: " <> show expects
              pure (t, r)
  where
    fmt t r = printf "Problem #%d: %.4f ms, %s\n" problemId t (show r)

cmdReport :: [String] -> IO ()
cmdReport _ = do
  results <- forM (IM.toAscList allProblems) $ \(_,p) -> evalProblem p
  let totalTime = kbn $ foldr (\(t,_) -> (`add` t)) zero results
      counts :: M.Map Result Int
      counts = M.fromListWith (+) $  (,1) . snd <$> results
      failedCount = fromMaybe 0 $
        (+) <$> M.lookup Wrong counts
            <*> M.lookup Crashed counts
  printf "Evaluated %d problems in %.4f ms.\n"
    (IM.size allProblems)
    totalTime
  forM_ (M.toAscList counts) $ \(r,count) ->
    when (count > 0) $
      printf "- %s: %d\n" (show r) count
  if failedCount == 0
    then exitSuccess
    else exitFailure
