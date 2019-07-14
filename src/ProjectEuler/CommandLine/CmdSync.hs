{-# LANGUAGE
    OverloadedStrings
  #-}
module ProjectEuler.CommandLine.CmdSync
  ( cmdSync
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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict
import qualified Data.FileEmbed

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
  as the starting line of the edit zone
  (hopefully it's also the same padding as the ending line of edit zone,
  but this function doesn't care about that.)

  (TODO: in future we'll require all contents in the edit zone
  to be left-padded by spaces exactly the same way)

  For example, a file with content:

  > outside.
  >     # ==== FOO_BEGIN
  >     whatever inside
  >     # ==== FOO_END
  > outside, after.

  will be updated if the function is called with

  - "FOO" as zoneIdent
  - ["aaa", "bb", "c"] as newContents

  to be:

  > outside.
  >     # ==== FOO_BEGIN
  >     aaa
  >     bb
  >     c
  >     # ==== FOO_END
  > outside, after.

 -}
updateEditZone :: String -> [String] -> String -> String
updateEditZone zoneIdent newContents =
    unlines . updateContentLines . lines
  where
    zoneBegin = "# ==== " <> zoneIdent <> "_BEGIN"
    zoneEnd = "# ==== " <> zoneIdent <> "_END"

    extractSectionBegin :: String -> Maybe String
    extractSectionBegin line = guard (content == zoneBegin) $> sps
      where
        (sps, content) = span (== ' ') line

    extractSectionEnd :: String -> Maybe ()
    extractSectionEnd line = guard $ content == zoneEnd
      where
        (_sps, content) = span (== ' ') line

    updateContentLines :: [String] -> [String]
    updateContentLines [] =
      -- we could have return [], but here we want to
      -- make sure that the edit zone is properly recognized.
      -- and if it does, this part is unreachable.
      error $ "Cannot find edit zone for " <> zoneIdent
    updateContentLines (x:xs) = case extractSectionBegin x of
      Just spChars ->
        let secAfter = dropWhile (isNothing . extractSectionEnd) xs
            updatedLines = (spChars <>) <$> newContents
        in x : updatedLines <> secAfter
      _ -> x : updateContentLines xs

updatePackageYaml :: FP.FilePath -> [Int] -> IO ()
updatePackageYaml projectHome pIds = do
    let fp = FP.encodeString $ projectHome </> "package.yaml"
    raw <- System.IO.Strict.readFile fp
    let moduleList = ("- ProjectEuler.Problem" <>) . show <$> pIds
    writeFile fp (updateEditZone "PROBLEM_MODULE_LIST" moduleList raw)

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
      (("- " <> ) <$> fPaths)
      raw

{-
  TODO: note that currently `pet sync` updates the files but won't try rebuilding the project,
  we could consider doing that automatically if possible.
 -}
cmdSync :: [String] -> IO ()
cmdSync _ = do
  prjHome <- getProjectHome
  pIds <- updateAllProblems prjHome
  updatePackageYaml prjHome pIds
  updateGetDataModule prjHome
