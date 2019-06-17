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
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP

import Common

_renderProblem :: FP.FilePath -> Int -> Bool -> T.Text -> IO TL.Text
_renderProblem tmplFP pId solved extraContent = do
  let ctxt = Object $ HM.fromList
        [ ("problem_id", Number $ fromIntegral pId)
        , ("solve_state", String (if solved then "Solved" else "Unsolved"))
        , ("extra_content", String extraContent)
        ]
  tmpl <- compileMustacheFile (FP.encodeString tmplFP)
  pure (renderMustache tmpl ctxt)

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
