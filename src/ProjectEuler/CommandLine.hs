{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  #-}
module ProjectEuler.CommandLine
  ( main
  ) where

import Control.Applicative
import Data.List
import System.Environment
import System.Exit

import qualified Data.Map.Strict as M

import ProjectEuler.CommandLine.CmdRun
import ProjectEuler.CommandLine.CmdCreate
import ProjectEuler.CommandLine.CmdSync
import ProjectEuler.CommandLine.CmdReport
import ProjectEuler.CommandLine.CmdBenchmark

{-
  Main binary is named "pet" for ProjectEuler Toolkit.
  Note that some sub commands would require
  environment variable "PROJECT_EULER_HOME" to point to project home directory.
 -}

{-
  first do a regular lookup, this allows us to do exact match
  if one of those sub commands happens to be the prefix of the other.
  then fallback to succeed as long as the given key
  matches exactly one result (by prefix)
 -}
uniqueLookup :: (Eq ke, Ord ke) => [ke] -> M.Map [ke] v -> Maybe v
uniqueLookup k m =
  M.lookup k m
  <|> case filter ((k `isPrefixOf`) . fst) $ M.toList m of
    [(_,v)] -> Just v
    _ -> Nothing

subCmds :: M.Map String ([String] -> IO ())
subCmds = M.fromList
  [ ( "run"
      {- `pet run <problem id>` executes solution for a program. -}
    , cmdRun
    )
  , ( "exec"
      {- same as `pet run` -}
    , cmdRun
    )
  , ( "new"
     {-
       `pet new <problem id>` sets up templates for a new problem.
      -}
    , cmdCreate
    )
  , ( "create"
      {- same as `pet new` -}
    , cmdCreate
    )
  , ( "sync"
      {-
        `pet sync` scans the directory to collect the list of problems
        and update related file accordingly.
        You shouldn't need to manually use this command.
       -}
    , cmdSync
    )
  , ( "report"
      {-
        `pet report` runs all problems marked as solved,
        make sure the output is expected, and report about time elapsed
        for each of the solutions.
       -}
    , cmdReport
    )
  , ( "benchmark"
      {- `pet benchmark <problem id> [...benchmark options...]` -}
    , cmdBenchmark
    )
  ]

{-
  planned features:
  - TODO: `pet good <problem id>` marks the solution to that problem as
    Solved, and record its output to answers.yaml
  - `pet data` invalidates stuff stored in `data/`.
    TODO: now I have a better plan: let's keep the list of files
    being loaded written down somewhere in the comment section of GetData file,
    so that whenever a file gets added, this list will update and should
    trigger the rebuild. Also in case a data file gets modified,
    the dependency should already be handled through file-embed internals
    so we don't need to worry about them.
 -}
main :: IO ()
main = getArgs >>= \case
  cmd:args' | Just action <- uniqueLookup cmd subCmds -> action args'
  _ -> do
    putStrLn $ "pet <" <> intercalate "|" (M.keys subCmds) <> ">"
    exitFailure
