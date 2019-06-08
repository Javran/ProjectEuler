{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import System.Exit
import System.Environment
import qualified Data.IntMap.Strict as IM
import ProjectEuler.Types
import ProjectEuler.AllProblems

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
          Just problem -> do
            putStrLn $ "Running Problem #" <> show pId <> ":"
            problemRun problem []
          Nothing -> do
            putStrLn $ "Problem #" <> show pId <> " not found."
            exitFailure
  _ -> error "TODO"

