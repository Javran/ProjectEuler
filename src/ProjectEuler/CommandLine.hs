{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  #-}
module ProjectEuler.CommandLine
  ( main
  ) where

import Control.Exception
import Control.Monad
import System.CPUTime
import System.Environment
import System.Exit
import Text.Printf
import Control.DeepSeq
import Control.Applicative
import Data.List

import qualified Data.Text.IO as T
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import ProjectEuler.GetData
import ProjectEuler.AllProblems
import ProjectEuler.Types
import ProjectEuler.CommandLine.CmdRun
import ProjectEuler.CommandLine.CmdRunAll

-- first do a regular lookup, fallback to
-- succeed as long as the given key matches exactly one result (by prefix)
uniqueLookup :: (Eq ke, Ord ke) => [ke] -> M.Map [ke] v -> Maybe v
uniqueLookup k m =
  M.lookup k m
  <|> case filter ((k `isPrefixOf`) . fst) $ M.toList m of
    [(_,v)] -> Just v
    _ -> Nothing

subCmds :: M.Map String ([String] -> IO ())
subCmds = M.fromList
  [ ("run", cmdRun)
  , ("run_all", cmdRunAll)
  ]


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
  - we need a custom test runner to surface slow solutions rather than
    using hspec. (`pet report` could do this, but I feel we need one
    specific for travis-ci to run)
  - merge templater's logic into this one,
    as the first step, perhaps we'll need to have a seperated submodule
    for command lines
 -}
main :: IO ()
main = getArgs >>= \case
  cmd:args' | Just action <- uniqueLookup cmd subCmds -> action args'
  _ -> do
    putStrLn $ "pet <" <> intercalate "|" (M.keys subCmds) <> ">"
    exitFailure
