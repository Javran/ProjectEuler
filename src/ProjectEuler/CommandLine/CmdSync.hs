{-# LANGUAGE
    OverloadedStrings
  #-}
module ProjectEuler.CommandLine.CmdSync
  ( cmdSync
  , updateEditZone
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Functor
import Data.List
import Data.Maybe
import Filesystem.Path.CurrentOS ((</>))
import Text.Microstache
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

import qualified Control.Foldl as Foldl
import qualified Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import qualified Data.List.Match as LMatch
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict

import ProjectEuler.CommandLine.Common

scanProblems :: FP.FilePath -> IO [Int]
scanProblems projectHome = do
    moduleFiles <-
      reduce Foldl.list $
        ls $ projectHome </> "src" </> "ProjectEuler"
    let moduleNames = either id id . FP.toText . FP.filename <$> moduleFiles
    pure $
      Data.List.sort
      . mapMaybe (listToMaybe . match patProblem)
      $ moduleNames
  where
    patProblem :: Pattern Int
    patProblem = text "Problem" *> (read <$> some digit) <* text ".hs"

renderAllProblemsContent :: Template -> [Int] -> TL.Text
renderAllProblemsContent tmpl pIds = renderMustache tmpl ctxt
  where
    ctxt :: Value
    ctxt = Object $ HM.fromList [("problem_list", problemList)]
    problemList =
        Array $ V.fromList $
          zipWith mk pIds (True:repeat False)
      where
        mk pId isFirst =
          Object $ HM.fromList
            [ ("first", Bool isFirst)
            , ("val", Number $ fromIntegral pId)
            ]

updateAllProblems :: FP.FilePath -> IO [Int]
updateAllProblems projectHome = do
  let allProblemsTmplPath =
        projectHome </> "data" </> "AllProblems.hs.mustache"
      allProblemsFilePath =
        projectHome </> "src" </> "ProjectEuler" </> "AllProblems.hs"
  template <- compileMustacheFile (FP.encodeString allProblemsTmplPath)
  pIds <- scanProblems projectHome
  let content = renderAllProblemsContent template pIds
  TL.writeFile (FP.encodeString allProblemsFilePath) content
  pure pIds

{-

  Recognize exactly one edit zone in a file, and replace
  the content inside edit zone with a list of text for each line,
  padded by spaces. The sequence of spaces used for padding is the same
  as the starting line of the edit zone.
  Note that this sequence of spaces should be used as prefix
  throughout the whole edit zone section.
  Extra spaces after the padding are fine - we only enforce
  this particular sequence of spaces needs to be a prefix of
  content lines (including "XXX_BEGIN" and "XXX_END" lines)

  For example, a file with content:

  > outside.
  >     # ==== FOO_BEGIN
  >     whatever inside
  >       foo
  >     # ==== FOO_END
  > outside, after.

  will be updated if the function is called with

  - "FOO" as zoneIdent
  - `const ["aaa", "bb", "c"]` as mkNewContents

  to be:

  > outside.
  >     # ==== FOO_BEGIN
  >     aaa
  >     bb
  >     c
  >     # ==== FOO_END
  > outside, after.

  In addition, the sequence of spaces are removed
  for mkNewContents to process,
  so mkNewContents will be called with following argument:

  ["whatever inside", "  foo"]

 -}
updateEditZone :: String -> ([String] -> [String]) -> String -> String
updateEditZone zoneIdent mkNewContents =
    unlines . updateContentLines . lines
  where
    zoneBegin = "# ==== " <> zoneIdent <> "_BEGIN"
    zoneEnd = "# ==== " <> zoneIdent <> "_END"

    extractSectionBegin :: String -> Maybe String
    extractSectionBegin line = guard (content == zoneBegin) $> sps
      where
        (sps, content) = span (== ' ') line

    extractSectionEnd :: String -> String -> Maybe ()
    extractSectionEnd expSps line =
        guard $ content == zoneEnd && sps == expSps
      where
        (sps, content) = span (== ' ') line

    updateContentLines :: [String] -> [String]
    updateContentLines [] =
      -- we could have return [], but here we want to
      -- make sure that the edit zone is properly recognized.
      -- and if it does, this part is unreachable.
      error $ "Cannot find edit zone for " <> zoneIdent
    updateContentLines (x:xs) = case extractSectionBegin x of
      Just spChars ->
        let (secContentsPadded, secAfter) =
              span (isNothing . extractSectionEnd spChars) xs
            unpadContentLine :: String -> String
            unpadContentLine raw =
                if actualSps == spChars
                  then unpadded
                  else error $ "unexpected prefix on line: " <> show raw
              where
                (actualSps, unpadded) = LMatch.splitAt spChars raw
            updatedLines =
              (spChars <>) <$>
                mkNewContents
                  (unpadContentLine <$> secContentsPadded)
        in x : updatedLines <> secAfter
      _ -> x : updateContentLines xs

updateGetDataModule :: FP.FilePath -> IO ()
updateGetDataModule prjHome = do
  let fpGetData =
        FP.encodeString $ prjHome </> "src" </> "ProjectEuler" </> "GetData.hs"
      fpDataDir =
        FP.encodeString $ prjHome </> "data"
  -- it's important that the list is sorted - we don't want to rely on
  -- the order that file system lists files in a directory.
  fPaths <- Data.List.sort . fmap fst <$> Data.FileEmbed.getDir fpDataDir
  raw <- System.IO.Strict.readFile fpGetData
  writeFile fpGetData $
    updateEditZone
      "DATA_FILE_LIST"
      (const $ ("- " <> ) <$> fPaths)
      raw

{-
  TODO: not sure whether try rebuilding again is a good idea -
  I'd say solution is incomplete and need some initial code to kick things off anyway,
  we might revisit this decision later.
 -}
cmdSync :: [String] -> IO ()
cmdSync _ = do
  prjHome <- getProjectHome
  pIds <- updateAllProblems prjHome
  updateGetDataModule prjHome
