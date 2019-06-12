{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import qualified Data.IntMap.Strict as IM
import ProjectEuler.AllProblems
import ProjectEuler.Types
import System.CPUTime
import System.Environment
import System.Exit
import Control.Exception
import Text.Printf

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
evalProblem Problem {problemId, problemRun} = do
  putStrLn $ "Evaluating Problem #" <> show problemId
  tStart <- getCPUTime
  catch @SomeException (problemRun []) $ \e ->
    putStrLn (displayException e)
  tEnd <- getCPUTime
  let diff = fromIntegral (tEnd - tStart) / (10^(9 :: Int))
  printf "Time elapsed: %0.4f ms\n" (diff :: Double)

{-
  main program is named "pet" for ProjectEuler Toolkit.

  TODO:

  - `pet run <problem id>` executes solution for a program.
  - `pet new <problem id>` sets up templates for a new problem.
    (TODO) this should be templater's job.
  - `pet run_all` for running all programs in sequence.

  TODO: problem args are not being respected
  TODO: plan for migrating old problems into new infra, we should still
    be able to see time reports in travis-ci.

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
  _ -> error "TODO"

