{-# LANGUAGE
    OverloadedStrings
  #-}
module CmdStat
  ( cmdStat
  ) where

import Control.Applicative
import Filesystem.Path.CurrentOS ((</>))
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Data.Maybe
import Data.List

import qualified Control.Foldl as Foldl
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP

import Common

patProblemId :: Pattern Int
patProblemId = do
  _ <- "Problem-"
  xs <- some digit
  _ <- ".hs"
  pure (read xs)

getProblemId :: T.Text -> Maybe Int
getProblemId = listToMaybe . match patProblemId

cmdStat :: [String] -> IO ()
cmdStat _ = do
  prjHome <- getProjectHome
  let oldProblemDirPath = FP.parent prjHome </> "solutions"
  fs <- reduce
    (mapMaybe (getProblemId . either id id . FP.toText . FP.filename) <$> Foldl.list)
    (ls oldProblemDirPath)
  putStrLn "Listing problem id of all old solutions:"
  putStrLn $ intercalate "," (show <$> Data.List.sort fs)
