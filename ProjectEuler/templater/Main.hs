{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Turtle.Prelude
import Turtle.Shell
import Turtle.Pattern
import Data.Maybe
import Text.Microstache
import Data.Aeson.QQ.Simple
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP
import qualified Control.Foldl as Foldl
import qualified Data.IntSet as IS

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


main :: IO ()
main = do
  curEnv <- env
  let Just projectHome = FP.fromText <$> lookup "PROJECT_EULER_HOME" curEnv
      allProblemsTmplPath =
        projectHome FP.</> "templater" FP.</> "mustache" FP.</> "AllProblems.hs"
  template <- compileMustacheFile (FP.encodeString allProblemsTmplPath)
  let v = [aesonQQ|
                    { "problem_list":
                      [{"first": true, "val": 1}, {"val": 2}, {"val": 6}]
                    , "timestamp": "covfefe"
                    }
                  |]
      out = renderMustache template v
  putStr (T.unpack . TL.toStrict $ out)
  print =<< scanProblems projectHome
