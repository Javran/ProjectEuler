{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  #-}
module ProjectEuler.Main
  ( main
  ) where

import Control.Exception
import Control.Monad
import System.CPUTime
import System.Environment
import System.Exit
import Text.Printf
import Control.DeepSeq

import qualified Data.Text.IO as T
import qualified Data.IntMap.Strict as IM

import ProjectEuler.GetData
import ProjectEuler.AllProblems
import ProjectEuler.Types

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
evalProblem Problem {problemId, problemRun, problemStatus} = do
  putStrLn $ "Evaluating Problem #" <> show problemId <> " ..."
  tStart <- getCPUTime
  r <- try @SomeException (runPEM problemRun >>= \((), outs) -> pure $!! outs)
  tEnd <- getCPUTime
  case r of
    Left e -> do
      -- note that if exception is uncaught, we will lose track of logs
      -- so it's recommended to make sure all lifted IO action are safe.
      putStrLn $ "Uncaught exception: " <> displayException e
      exitFailure
    Right outs -> do
      putStrLn "Output:"
      mapM_ T.putStrLn outs
      let diff = fromIntegral (tEnd - tStart) / (10^(9 :: Int))
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

{-
  main program is named "pet" for ProjectEuler Toolkit.

  - `pet run <problem id>` executes solution for a program.
  - `pet run_all` for running all programs in sequence.

  to be implemented:
  - `pet new <problem id>` sets up templates for a new problem.
    TODO: after migration is done, we'll remove templater and
      move its useful functions over here.
  - `pet good <problem id>` marks the solution to that problem as
    Solved, and record its output to answers.yaml
  - `pet stat` statistics (# of solved, # of unsolved, total, etc.)
  - `pet report` this one will eventualy replace `pet run_all`, to generate
    report for all existing solutions (mainly time report).
  - allow smart completion like templater does.
  - we need a custom test runner to surface slow solutions rather than
    using hspec. (`pet report` could do this, but I feel we need one
    specific for travis-ci to run)
  - merge templater's logic into this one,
    as the first step, perhaps we'll need to have a seperated submodule
    for command lines
 -}

main :: IO ()
main = getArgs >>= \case
  ["run", idStr]
    | [(pId, "")] <- reads idStr ->
        case IM.lookup pId allProblems of
          Just problem -> evalProblem problem
          Nothing -> do
            putStrLn $ "Problem #" <> show pId <> " not found."
            exitFailure
  ["run_all"] -> forM_ (IM.toAscList allProblems) $ \(_,p) -> evalProblem p
  xs -> error $ "Unrecognized: " <> show xs
