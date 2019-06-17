{-# LANGUAGE
    OverloadedStrings
  #-}
module Common
  ( getProjectHome
  ) where

import Turtle.Prelude
import qualified Filesystem.Path.CurrentOS as FP

getProjectHome :: IO FP.FilePath
getProjectHome = do
  curEnv <- env
  let Just projectHome = FP.fromText <$> lookup "PROJECT_EULER_HOME" curEnv
  pure projectHome
