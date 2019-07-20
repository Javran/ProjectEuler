{-# LANGUAGE
    TypeApplications
  #-}
module ProjectEuler.CommandLine.CmdGood
  ( cmdGood
  ) where

import Control.Exception
import Data.Char
import Data.Maybe
import System.Exit
import Text.ParserCombinators.ReadP

import qualified Data.IntMap as IM
import qualified Data.List.Match as LMatch
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict

import ProjectEuler.AllProblems
import ProjectEuler.CommandLine.CmdSync (updateEditZone)
import ProjectEuler.CommandLine.Common
import ProjectEuler.CommandLine.ParseAnswers

{-
  `pet good <problem id>` marks the solution to that problem as
  Solved, and record its output to answers.yaml
 -}

-- attempt to recognize "Unsolved" or "Solved" in a single line
-- and modify it to be "Solved" if possible.
markSolved :: String -> Maybe (String, Bool)
markSolved inp = case readP_to_S (parse <* eof) inp of
    [(r, "")] -> Just r
    _ -> Nothing
  where
    parse = do
      blk0 <- many get
      mark <- string "Unsolved" <++ string "Solved"
      blk1 <- many get
      pure (blk0 <> "Solved" <> blk1, mark == "Unsolved")

{-
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
    for this to work we use `updateEditZone` from CmdSync to be exported
    and extended that function to allow modifying base on original contents.
 -}
modifyProblemDefinition :: [String] -> Either String ([String], Bool)
modifyProblemDefinition xs = case catMaybes ys of
    [(_, actuallyModified)] ->
      let merge l Nothing = l
          merge l (Just (_, False)) = l
          merge _ (Just (r, True)) = r
      in pure (zipWith merge xs ys, actuallyModified)
    _ -> Left "Expected exactly one occurrence of either `Unsolved` or `Solved`."
  where
    ys = markSolved <$> xs

{-
  this function modified the content of solution file,
  returns the content after modification, together with
  a Bool indicating whether the content actually have changed.
  As it could be the case that the solution is already marked
  as Solved, in which case we don't need to do anything.
 -}
modifySolutionFileContent :: String -> Either String (String, Bool)
modifySolutionFileContent raw = do
    let inp0 = lines raw
    (blk0, inp1) <- consume0 inp0 []
    (blk1, remained) <- consume1 inp1 []
    (blk1', actuallyModified) <- modifyProblemDefinition blk1
    pure (unlines (blk0 <> blk1' <> remained), actuallyModified)
  where
    isProblemTySigLine x = xs == tySigLine && all isSpace ys
      where
        tySigLine = "problem :: Problem"
        (xs, ys) = LMatch.splitAt tySigLine x
    -- consume input lines until we get type signature line of `problem`.
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

updateRawContentWithNewAnswer :: Int -> [String] -> [String] -> [String]
updateRawContentWithNewAnswer pId answerLines rawLines =
    foldMap pprPair (IM.toAscList answers)
  where
    pprPair (k,[v]) = [show k <> ": " <> v]
    pprPair (k,vs) =
      (show k <> ":") : (("  - " <> ) <$> vs)

    answers :: IM.IntMap [String]
    answers =
      IM.insert pId answerLines
      . IM.fromList
      $ parseAnswersSection (unlines rawLines)

runAndRecordAnswerLines :: Int -> IO [T.Text]
runAndRecordAnswerLines pId = case IM.lookup pId allProblems of
  Nothing -> do
    putStrLn $ "Problem #" <> show pId <> " not found."
    exitFailure
  Just problem -> do
    putStrLn $ "Executing Problem #" <> show pId <> "..."
    (_, r) <- runProblem problem
    case r of
      Left e -> do
        putStrLn $ displayException e
        exitFailure
      Right xs ->
        -- unlines then lines. this will make sure no newline
        -- appears in string content.
        pure . T.lines . T.unlines $ xs

cmdGood :: [String] -> IO ()
cmdGood xs
  | [rawN] <- xs
  , [(pId,"")] <- reads @Int rawN
  = do
      prjHome <- getProjectHome
      let fpSol = FP.encodeString $ solutionPath prjHome pId
          fpAns = FP.encodeString $ answersPath prjHome

      {-
        We execute the problem and update data/answers.yaml first,
        this is to:
        - confirm that current solution is working
        - even if we failed to parse & modify the mark for some reason,
          we will get the reminder for updating the mark from `pet exec` anyways.
       -}
      outputLines <- runAndRecordAnswerLines pId
      putStrLn "++++ BEGIN ++++"
      mapM_ T.putStrLn outputLines
      putStrLn "---- END ----"
      do
        raw <- System.IO.Strict.readFile fpAns
        let newContent =
              updateEditZone
                "ANSWERS_LIST"
                (updateRawContentWithNewAnswer pId (T.unpack <$> outputLines))
                raw
        if newContent == raw
          then putStrLn "No change necessary to answers.yaml."
          else do
            writeFile fpAns newContent
            putStrLn $ "Answer for Problem #" <> show pId <> " is now recorded."
      do
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
                writeFile fpSol raw'
                putStrLn "Solution file updated."
              else putStrLn "No change necessary to the solution file."
  | otherwise = do
      putStrLn "pet create <problem id>"
      exitFailure
