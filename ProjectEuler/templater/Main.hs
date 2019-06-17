{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  #-}
module Main where

import Data.List
import System.Environment
import System.Exit

import qualified Data.Map.Strict as M

import CmdSync
import CmdMigrate

{-
  The purpose of templater is to ... well, apply templates.
  Having a standalone program to scan through files and generate modules
  accordingly for us will give us more control than using TemplateHaskell.
 -}

{-

  Usage: require environment variable "PROJECT_EULER_HOME" to point to project home directory.

  TODO: plan to bring up "sync" functionality first and continue to allow creating
  problem modules with template.

  - `templater create <num>`: create Problem<num> using template. (implies `sync`)
  - `templater migrate <num>`: migrate a old solution code. (implies `sync`)
    Note that this is by no means a correct migration - this only move
    the file with proper naming and program backbone, which will in turn
    result in less repetitive work.
  - `templater sync`:
    + scan through problems and re-generate AllProblems.hs.
    + upate package.yaml and update the list of problem modules

 -}

-- succeed as long as the given key matches exactly one result (by prefix)
uniqueLookup :: Eq ke => [ke] -> M.Map [ke] v -> Maybe v
uniqueLookup k m = case filter ((k `isPrefixOf`) . fst) $ M.toList m of
  [(_,v)] -> Just v
  _ -> Nothing

subCmds :: M.Map String ([String] -> IO ())
subCmds = M.fromList
  [ ("migrate", cmdMigrate)
  , ("create", const $ pure ()) -- TODO
  , ("sync", cmdSync)
  ]

main :: IO ()
main = getArgs >>= \case
  cmd:args' | Just action <- uniqueLookup cmd subCmds -> action args'
  _ -> do
    putStrLn $ "templater <" <> intercalate "|" (M.keys subCmds) <> ">"
    exitFailure
