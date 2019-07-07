{-# LANGUAGE OverloadedStrings #-}
module ProjectEuler.CommandLine.CmdBenchmark
  ( cmdBenchmark
  ) where

import Criterion.Main
import System.CPUTime
import System.Environment
import System.Exit
import TextShow
import Turtle.Prelude

import qualified Data.IntMap.Strict as IM

import ProjectEuler.AllProblems
import ProjectEuler.CommandLine.Common

{-
  This sub-command wraps around criterion to provide benchmark features.
 -}

{-

  Note: we benchmark the program by running solution as an external binary.
  By doing so we make sure GHC doesn't being too smart and just reuse results
  among execution of the same IO action.

 -}
runProblemAsExternalBinary :: Int -> IO Double
runProblemAsExternalBinary pId = do
  tStart <- getCPUTime
  (ExitSuccess, _) <- procStrict "./pet" ["run", showt pId] ""
  tEnd <- getCPUTime
  pure $ fromIntegral (tEnd - tStart) / (10^(9 :: Int))

cmdBenchmark :: [String] -> IO ()
cmdBenchmark args
  | idStr:benchArgs <- args
  , [(pId, "")] <- reads idStr =
    case IM.lookup pId allProblems of
      Just _ -> do
        prjHome <- getProjectHome
        cd prjHome
        withArgs benchArgs $
          defaultMain
            [bench ("Problem #" <> show pId) $ nfAppIO runProblemAsExternalBinary pId]
      Nothing -> do
        putStrLn $ "Problem #" <> show pId <> " not found."
        exitFailure
  | otherwise =
      putStrLn "pet benchmark <problem id> [...benchmark args...]"
