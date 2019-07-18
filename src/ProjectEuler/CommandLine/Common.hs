{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , TypeApplications
  #-}
module ProjectEuler.CommandLine.Common
  ( getProjectHome
  , renderProblem
  , runProblem
  , solutionPath
  , answersPath
  ) where

import Control.DeepSeq
import Control.Exception
import Data.Aeson
import Filesystem.Path.CurrentOS ((</>))
import System.CPUTime
import System.Exit
import Text.Microstache
import TextShow
import Turtle.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP

import ProjectEuler.Types

getProjectHome :: IO FP.FilePath
getProjectHome = do
  let varName = "PROJECT_EULER_HOME"
  curEnv <- env
  case FP.fromText <$> lookup varName curEnv of
    Just projectHome -> pure projectHome
    Nothing -> do
      T.putStrLn $ varName <> " is not set."
      exitFailure

renderProblem :: Int -> Bool -> T.Text -> IO TL.Text
renderProblem pId solved extraContent = do
  prjHome <- getProjectHome
  let tmplFP = prjHome </> "data" </> "ProblemX.hs.mustache"
      ctxt = Object $ HM.fromList
        [ ("problem_id", Number $ fromIntegral pId)
        , ("solve_state", String (if solved then "Solved" else "Unsolved"))
        , ("extra_content", String extraContent)
        ]
  tmpl <- compileMustacheFile (FP.encodeString tmplFP)
  pure (renderMustache tmpl ctxt)

{-
  Run a problem to its completion, measure time and capture output and exceptions.
  The time is in microseconds, and if problem runs successfully,
  the output will be fully reduced to normal form before returning.
 -}
runProblem :: Problem -> IO (Double, Either SomeException [T.Text])
runProblem Problem {problemRun} = do
  tStart <- getCPUTime
  r <- try @SomeException (runPEM problemRun >>= \((), outs) -> pure $!! outs)
  tEnd <- getCPUTime
  let diff = fromIntegral (tEnd - tStart) / (10^(9 :: Int))
  diff `deepseq` case r of
    Left e -> pure (diff, Left e)
    Right outs -> pure (diff, Right outs)

solutionPath :: FP.FilePath -> Int -> FP.FilePath
solutionPath prjHome pId =
  prjHome </> "src" </> "ProjectEuler"
    </> FP.fromText ("Problem" <> showt pId <> ".hs")

answersPath :: FP.FilePath -> FP.FilePath
answersPath prjHome =
  prjHome </> "data" </> "answers.yaml"
