{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
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
import Text.Printf

evalProblem :: Problem -> IO ()
evalProblem Problem {problemId, problemRun} = do
  putStrLn $ "Evaluating Problem #" <> show problemId
  tStart <- getCPUTime
  problemRun []
  tEnd <- getCPUTime
  let diff = fromIntegral (tEnd - tStart) / (10^(9 :: Int))
  printf "Time elapsed: %0.4f ms\n" (diff :: Double)

{-
  main program is named "pet" for ProjectEuler Toolkit.

  TODO:

  - `pet run <problem id>` executes solution for a program.
  - `pet new <problem id>` sets up templates for a new problem.

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

