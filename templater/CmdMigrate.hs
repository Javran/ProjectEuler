{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module CmdMigrate
  ( cmdMigrate
  ) where

import Filesystem.Path.CurrentOS ((</>))
import System.Exit
import TextShow
import Turtle.Prelude

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Filesystem.Path.CurrentOS as FP

import Common
import CmdSync

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
      oldProblemContents <- T.readFile (FP.encodeString oldProblemPath)
      contents <- renderProblem pId True oldProblemContents
      TL.writeFile (FP.encodeString newProblemPath) contents
      putStrLn "Problem migrated from:"
      putStrLn $ "  " <> FP.encodeString oldProblemPath
      putStrLn "To:"
      putStrLn $ "  " <> FP.encodeString newProblemPath
      rm oldProblemPath
      cmdSync []
  | otherwise = do
      putStrLn "templater migrate <problem id>"
      exitFailure
