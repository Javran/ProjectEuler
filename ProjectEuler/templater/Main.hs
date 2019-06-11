{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time.Clock
import Filesystem.Path.CurrentOS ((</>))
import Text.Microstache
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

import qualified Control.Foldl as Foldl
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.IO.Strict

{-
  The purpose of templater is to ... well, apply templates.
  Having a standalone program to scan through files and generate modules
  accordingly for us will give us more control than using TemplateHaskell.
 -}

{-

  Usage: require environment variable "PROJECT_EULER_HOME" to point to project home directory.

  TODO: plan to bring up "sync" functionality first and continue to allow creating
  problem modules with template.

  - `templater <num>`: create Problem<num> using template (implies `sync`)
  - `templater sync`:
    + scan through problems and re-generate AllProblems.hs.
    + upate package.yaml and update the list of problem modules

 -}

scanProblems :: FP.FilePath -> IO [Int]
scanProblems projectHome = do
    moduleFiles <-
      reduce Foldl.list $
        ls $ projectHome FP.</> "src" FP.</> "ProjectEuler"
    let moduleNames = either id id . FP.toText . FP.filename <$> moduleFiles
    pure $
      Data.List.sort
      . mapMaybe (listToMaybe . match patProblem)
      $ moduleNames
  where
    patProblem :: Pattern Int
    patProblem = text "Problem" *> (read <$> some digit) <* text ".hs"

renderAllProblemsContent :: UTCTime -> Template -> [Int] -> TL.Text
renderAllProblemsContent t tmpl pIds = renderMustache tmpl ctxt
  where
    ctxt :: Value
    ctxt = Object $ HM.fromList
      [ ("timestamp", String (T.pack . show $ t))
      , ("problem_list", problemList)
      ]
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
        projectHome </> "templater" </> "mustache" </> "AllProblems.hs"
      allProblemsFilePath =
        projectHome </> "src" </> "ProjectEuler" </> "AllProblems.hs"
  template <- compileMustacheFile (FP.encodeString allProblemsTmplPath)
  pIds <- scanProblems projectHome
  t <- getCurrentTime
  let content = renderAllProblemsContent t template pIds
  TL.writeFile (FP.encodeString allProblemsFilePath) content
  pure pIds

updatePackageYaml :: FP.FilePath -> [Int] -> IO ()
updatePackageYaml projectHome pIds = do
    let fp = FP.encodeString $ projectHome </> "package.yaml"
    raws <- lines <$> System.IO.Strict.readFile fp
    let newContent = unlines . updatePackageYamlContent $ raws
    writeFile fp newContent
  where
    extractSectionBegin :: String -> Maybe String
    extractSectionBegin line = do
        "# ==== PROBLEM_MODULE_LIST_BEGIN" <- pure content
        pure sps
      where
        (sps, content) = span (== ' ') line
    extractSectionEnd :: String -> Maybe ()
    extractSectionEnd line = do
        "# ==== PROBLEM_MODULE_LIST_END" <- pure content
        pure ()
      where
        (_sps, content) = span (== ' ') line

    updatePackageYamlContent :: [String] -> [String]
    updatePackageYamlContent [] =
      -- we could have return [], but here we want to
      -- make sure that the section for templater is properly recognized.
      -- and if it does, this part is unreachable.
      error "package.yaml is empty"
    updatePackageYamlContent (x:xs)
      | Just spChars <- extractSectionBegin x =
          let secAfter = dropWhile (isNothing . extractSectionEnd) xs
              problemModules =
                (spChars <>) . ("- ProjectEuler.Problem" <>) . show
                  <$> pIds
          in x:problemModules <> secAfter
      | otherwise = x : updatePackageYamlContent xs

main :: IO ()
main = do
  curEnv <- env
  let Just projectHome = FP.fromText <$> lookup "PROJECT_EULER_HOME" curEnv
  pIds <- updateAllProblems projectHome
  updatePackageYaml projectHome pIds
