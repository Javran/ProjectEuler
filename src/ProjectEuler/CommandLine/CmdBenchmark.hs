module ProjectEuler.CommandLine.CmdBenchmark
  ( cmdBenchmark
  ) where

import qualified Data.IntMap.Strict as IM

import Control.Exception
import Criterion.Main
import System.Environment
import System.Exit

import qualified Data.Text as T

import ProjectEuler.GetData
import ProjectEuler.AllProblems

import ProjectEuler.CommandLine.Common

{-
  This sub-command wraps around criterion to provide benchmark features.
 -}

cmdBenchmark :: [String] -> IO ()
cmdBenchmark args
  | idStr:benchArgs <- args
  , [(pId, "")] <- reads idStr =
    case IM.lookup pId allProblems of
      Just problem -> do
        let solRun :: IO (Double, [T.Text])
            solRun = do
              (t, r) <- runProblem problem
              case r of
                Left e -> throw e
                Right outs ->
                  case getExpectedAnswers pId of
                    Nothing -> pure (t,outs)
                    Just expectedOuts ->
                      if expectedOuts == outs
                        then pure (t,outs)
                        else error "Unexpected output."
        withArgs benchArgs $
          defaultMain
            [bench ("Problem #" <> show pId) $ nfIO solRun]
      Nothing -> do
        putStrLn $ "Problem #" <> show pId <> " not found."
        exitFailure
  | otherwise =
      putStrLn "pet benchmark <problem id> [...benchmark args...]"
