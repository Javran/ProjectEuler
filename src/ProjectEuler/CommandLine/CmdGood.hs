{-# LANGUAGE
    TypeApplications
  #-}
module ProjectEuler.CommandLine.CmdGood
  ( cmdGood
  ) where

import System.Exit
import Data.Char

import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict
import qualified Data.List.Match as LMatch

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
modifySolutionFileContent :: String -> Either String (String, Bool)
modifySolutionFileContent raw = do
    let inp0 = lines raw
    (blk0,inp1) <- consume0 inp0 []
    (blk1,remained) <- consume1 inp1 []
    pure (unlines (blk0 <> ["<start>"] <> blk1 <> ["<end>"] <> remained), True)
  where
    isProblemTySigLine x = xs == tySigLine && all isSpace ys
      where
        tySigLine = "problem :: Problem"
        (xs, ys) = LMatch.splitAt tySigLine x
    -- first step: consume input lines until we get type signature line of `problem`.
    consume0 [] _ = Left "cannot find type signature for `problem`."
    consume0 (x:xs) acc =
      if isProblemTySigLine x
        then pure (reverse acc', xs)
        else consume0 xs acc'
      where
        acc' = x:acc
    -- then consume body of `problem`.
    consume1 [] acc = pure (reverse acc, [])
    consume1 (x:xs) acc =
      let acc' = x : acc
      in if all isSpace x
        then pure (reverse acc', xs)
        else consume1 xs acc'

cmdGood :: [String] -> IO ()
cmdGood xs
  | [rawN] <- xs
  , [(pId,"")] <- reads @Int rawN
  = do
      prjHome <- getProjectHome
      let fpSol = FP.encodeString $ solutionPath prjHome pId
      raw <- System.IO.Strict.readFile fpSol
      case modifySolutionFileContent raw of
        Left reason -> do
          putStrLn $
            "Failed to modify " <> fpSol <> " , unexpected file content."
          putStrLn $ "Error: " <> reason
          exitFailure
        Right (raw', actuallyChanged) ->
          if actuallyChanged
            then do
              putStrLn raw'
              -- TODO: actually write to the file.
              -- writeFile fpSol raw'
              putStrLn "Solution file updated."
            else putStrLn "No change necessary to the solution file."
      -- TODO: execute & record result in data/answers.yaml
      pure ()

  | otherwise = do
      putStrLn "pet create <problem id>"
      exitFailure
