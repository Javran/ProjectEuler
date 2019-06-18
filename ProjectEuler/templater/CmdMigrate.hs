{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module CmdMigrate
  ( cmdMigrate
  ) where

import Data.Aeson
import Filesystem.Path.CurrentOS ((</>))
import System.Exit
import Text.Microstache
import TextShow

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP

import Common

cmdMigrate :: [String] -> IO ()
cmdMigrate xs
  | [rawN] <- xs
  , [(pId,"")] <- reads @Int rawN
  = do
      prjHome <- getProjectHome
      let oldProblemPath =
            FP.parent prjHome </> "solutions"
            </> FP.fromText ("Problem-" <> showt pId <> ".hs")
          newProblemPath =
            prjHome </> "src" </> "ProjectEuler"
            </> FP.fromText ("Problem" <> showt pId <> ".hs")
      putStrLn "Content: "
      oldProblemContents <- T.readFile (FP.encodeString oldProblemPath)
      contents <- renderProblem pId True oldProblemContents
      T.putStrLn (TL.toStrict contents)
      print newProblemPath
      -- TODO: fill template here
  | otherwise = do
      putStrLn "templater migrate <problem id>"
      exitFailure
