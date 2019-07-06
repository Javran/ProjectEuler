{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , TypeApplications
  #-}
module ProjectEuler.CommandLine.Common
  ( getProjectHome
  , renderProblem
  , runProblem
  ) where

import Control.DeepSeq
import Control.Exception
import Data.Aeson
import Filesystem.Path.CurrentOS ((</>))
import System.CPUTime
import Text.Microstache
import Turtle.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP

import ProjectEuler.Types

getProjectHome :: IO FP.FilePath
getProjectHome = do
  curEnv <- env
  let Just projectHome = FP.fromText <$> lookup "PROJECT_EULER_HOME" curEnv
  pure projectHome

renderProblem :: Int -> Bool -> T.Text -> IO TL.Text
renderProblem pId solved extraContent = do
  prjHome <- getProjectHome
  -- TODO: perhaps we should have a more standard dir structure
  -- so that all templates can be loaded at once.
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
