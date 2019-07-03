{-# LANGUAGE
    OverloadedStrings
  #-}
module Common
  ( getProjectHome
  , renderProblem
  ) where

import Data.Aeson
import Filesystem.Path.CurrentOS ((</>))
import Text.Microstache
import Turtle.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP


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
