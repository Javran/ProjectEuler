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
import ProjectEuler.CommandLine.CmdRunAll
import ProjectEuler.CommandLine.CmdCreate
import ProjectEuler.CommandLine.CmdSync


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
  , ("create", cmdCreate)
  , ("sync", cmdSync)
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

{-
  TODO: the following stuff is migrated from templater, and might not make
  sense anymore, we need to revisit them in future.
 -}

{-
  The purpose of templater is to ... well, apply templates.
  Having a standalone program to scan through files and generate modules
  accordingly for us will give us more control than using TemplateHaskell.
 -}

{-

  Usage: require environment variable "PROJECT_EULER_HOME" to point to project home directory.

  - `templater sync`:

    + scan through problems and re-generate AllProblems.hs.
    + update package.yaml and update the list of problem modules

  - `templater create <num>`: create Problem<num> using template. (implies `sync`)

  - `templater migrate <num>`: migrate a old solution code. (implies `sync`)

    Note that this is by no means a correct migration - this only move
    the file with proper naming and program backbone, which will in turn
    result in less repetitive work.

  - `templater stat`: show the list of not-yet-migrated solutions.

 -}
