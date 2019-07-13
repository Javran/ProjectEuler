{-# LANGUAGE
    OverloadedStrings
  #-}
module ProjectEuler.CommandLine.CmdSync
  ( cmdSync
  ) where

import Control.Applicative
import Data.Aeson
import Data.List
import Data.Maybe
import Filesystem.Path.CurrentOS ((</>))
import Text.Microstache
import Turtle.Pattern
import Turtle.Prelude
import Control.Monad
import Turtle.Shell

import qualified Control.Foldl as Foldl
import qualified Data.HashMap.Strict as HM
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

updateEditZone :: String -> [String] -> String -> String
updateEditZone zoneIdent newContents =
    unlines . updateContentLines . lines
  where
    zoneBegin = "# ==== " <> zoneIdent <> "_BEGIN"
    zoneEnd = "# ==== " <> zoneIdent <> "_END"

    extractSectionBegin :: String -> Maybe String
    extractSectionBegin line = do
        guard $ content == zoneBegin
        pure sps
      where
        (sps, content) = span (== ' ') line

    extractSectionEnd :: String -> Maybe ()
    extractSectionEnd line =
        guard $ content == zoneEnd
      where
        (_sps, content) = span (== ' ') line

    updateContentLines :: [String] -> [String]
    updateContentLines [] =
      -- we could have return [], but here we want to
      -- make sure that the section for templater is properly recognized.
      -- and if it does, this part is unreachable.
      error $ "Cannot find edit zone for " <> zoneIdent
    updateContentLines (x:xs) = case extractSectionBegin x of
      Just spChars ->
        let secAfter = dropWhile (isNothing . extractSectionEnd) xs
            problemModules = (spChars <>) <$> newContents
        in x : problemModules <> secAfter
      _ -> x : updateContentLines xs

updatePackageYaml :: FP.FilePath -> [Int] -> IO ()
updatePackageYaml projectHome pIds = do
    let fp = FP.encodeString $ projectHome </> "package.yaml"
    raw <- System.IO.Strict.readFile fp
    let moduleList = ("- ProjectEuler.Problem" <>) . show <$> pIds
    writeFile fp (updateEditZone "PROBLEM_MODULE_LIST" moduleList raw)

cmdSync :: [String] -> IO ()
cmdSync _ = do
  prjHome <- getProjectHome
  pIds <- updateAllProblems prjHome
  updatePackageYaml prjHome pIds
