{-# LANGUAGE
    NamedFieldPuns
  #-}
module ProjectEuler.CommandLine.CmdRun
  ( evalProblem
  , cmdRun
  ) where

import Control.Exception
import Control.Monad
import System.Exit
import Text.Printf

import qualified Data.Text.IO as T
import qualified Data.IntMap.Strict as IM

import ProjectEuler.GetData
import ProjectEuler.AllProblems
import ProjectEuler.Types

import ProjectEuler.CommandLine.Common

{-
  Run the program and measure time.

  Note that this is by no means an accurate execution time,
  as we have intentionally put all solution programs together,
  some sharing will happen, making overall time less than
  running each of them individually.

  However, this still helps in detecting solutions that
  takes a long time to run and therefore surface potential points
  of improvement.
 -}
evalProblem :: Problem -> IO ()
evalProblem p@Problem {problemId, problemStatus} = do
  putStrLn $ "Evaluating Problem #" <> show problemId <> " ..."
  (diff, r) <- runProblem p
  case r of
    Left e -> do
      -- note that if exception is uncaught, we will lose track of logs
      -- so it's recommended to make sure all lifted IO action are safe.
      putStrLn $ "Uncaught exception: " <> displayException e
      exitFailure
    Right outs -> do
      putStrLn "Output:"
      mapM_ T.putStrLn outs
      printf "Time elapsed: %0.4f ms\n" (diff :: Double)
      case getExpectedAnswers problemId of
        Nothing -> pure ()
        Just expectedOuts ->
          if expectedOuts == outs
            then do
              putStrLn "The output matches the expected answer."
              when (problemStatus == Unsolved) $
                putStrLn "Note: This problem is still marked as `Unsolved`."
            else do
              putStrLn "The output does not match the expected answer."
              putStrLn "Expected:"
              mapM_ T.putStrLn expectedOuts
              exitFailure

cmdRun :: [String] -> IO ()
cmdRun args
  | [idStr] <- args
  , [(pId, "")] <- reads idStr =
    case IM.lookup pId allProblems of
      Just problem -> evalProblem problem
      Nothing -> do
        putStrLn $ "Problem #" <> show pId <> " not found."
        exitFailure
  | otherwise = do
      putStrLn $ "Unknown args: " <> unwords args
      exitFailure
