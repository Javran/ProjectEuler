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
      putStrLn "From: "
      print oldProblemPath
      putStrLn "To: "
      print newProblemPath
      -- TODO: fill template here
  | otherwise = do
      putStrLn "templater migrate <problem id>"
      exitFailure
