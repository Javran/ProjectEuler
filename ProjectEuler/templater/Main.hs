{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle.Prelude
import Turtle.Shell
import Turtle.Pattern
import Data.Maybe
import Text.Microstache
import Data.Aeson
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP
import qualified Control.Foldl as Foldl
import qualified Data.IntSet as IS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

{-
  The purpose of templater is to ... well, apply templates.
  Having a standalone program to scan through files and generate modules
  accordingly for us will give us more control than using TemplateHaskell.
 -}

{-

  Usage: require environment variable "PROJECT_EULER_HOME" to point to project home directory.

  - `templater <num>`: create Problem<num> using template (implies `sync`)
  - `templater sync`:
    + scan through problems and re-generate AllProblems.hs.
    + upate package.yaml and update the list of problem modules

 -}


scanProblems :: FP.FilePath -> IO IS.IntSet
scanProblems projectHome = do
    moduleFiles <-
      reduce Foldl.list $
        ls $ projectHome FP.</> "src" FP.</> "ProjectEuler"
    let moduleNames = either id id . FP.toText . FP.filename <$> moduleFiles
    pure $
      IS.fromList
      . mapMaybe (listToMaybe . match patProblem)
      $ moduleNames
  where
    patProblem :: Pattern Int
    patProblem = text "Problem" *> (read <$> some digit) <* text ".hs"

renderAllProblemsContent :: Template -> IS.IntSet -> TL.Text
renderAllProblemsContent tmpl pIds = renderMustache tmpl ctxt
  where
    ctxt :: Value
    ctxt = Object $ HM.fromList
      [ ("timestamp", "covfefe")
      , ("problem_list", problemList)
      ]
    problemList =
        Array $ V.fromList $
          zipWith mk (IS.toAscList pIds) (True:repeat False)
      where
        mk pId isFirst =
          Object $ HM.fromList
            [ ("first", Bool isFirst)
            , ("val", Number $ fromIntegral pId)
            ]

main :: IO ()
main = do
  curEnv <- env
  let Just projectHome = FP.fromText <$> lookup "PROJECT_EULER_HOME" curEnv
      allProblemsTmplPath =
        projectHome FP.</> "templater" FP.</> "mustache" FP.</> "AllProblems.hs"
  template <- compileMustacheFile (FP.encodeString allProblemsTmplPath)
  pIds <- scanProblems projectHome
  let out = renderAllProblemsContent template pIds
  putStr (T.unpack . TL.toStrict $ out)
