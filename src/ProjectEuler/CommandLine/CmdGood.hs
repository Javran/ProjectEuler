{-# LANGUAGE
    TypeApplications
  #-}
module ProjectEuler.CommandLine.CmdGood
  ( cmdGood
  ) where

import System.Exit

import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict

import ProjectEuler.CommandLine.Common

{-
  `pet good <problem id>` marks the solution to that problem as
  Solved, and record its output to answers.yaml

  TOOD: not implemented yet.
 -}

{-
  Implementation draft:

  - for marking a problem as solved:
    we just need something quick and simple, I'll
    going to assume that all solutions are formatted in the following way:

    > problem :: Problem
    > <... some contents (maybe several lines, but must be non-empty ...>
    > <end of file or an empty line>


    Also the string "Unsolved" is present exactly once in this section.

    With this assumption the update should be straightforward to do
    using following steps:

    + find the line that equals "problem :: Problem" (trailing spaces allowed)
    + keep consuming lines until hitting end of file or an empty line
      (spaces are ignored)
    + in the consumed section, there should be exactly one occurrence of string "Unsolved",
      change that into "Solved", and write other content back without change.

    Note that as a test we can go through all existing problems and try to parse
    all of them and see if we have some missing cases.

  - for recording the current solution: execute, then update data/answers.yaml
    for this to work we'll need `updateEditZone` from CmdSync to be exported
    and extend that function to allow modifying base on original contents.
 -}

{-
  TODO: impl
  this function modified the content of solution file,
  returns the content after modification, together with
  a Bool indicating whether the content actually have changed.
  As it could be the case that the solution is already marked
  as Solved, in which case we don't need to do anything.
 -}
modifySolutionFileContent :: String -> Maybe (String, Bool)
modifySolutionFileContent _ = Nothing

cmdGood :: [String] -> IO ()
cmdGood xs
  | [rawN] <- xs
  , [(pId,"")] <- reads @Int rawN
  = do
      prjHome <- getProjectHome
      let fpSol = FP.encodeString $ solutionPath prjHome pId
      raw <- System.IO.Strict.readFile fpSol
      case modifySolutionFileContent raw of
        Nothing -> do
          putStrLn $
            "Failed to modify " <> fpSol <> " , unexpected file content."
          exitFailure
        Just (raw', actuallyChanged) ->
          if actuallyChanged
            then do
              writeFile fpSol raw'
              putStrLn "Solution file updated."
            else putStrLn "No change necessary to the solution file."
      -- TODO: execute & record result in data/answers.yaml
      pure ()

  | otherwise = do
      putStrLn "pet create <problem id>"
      exitFailure
