module ProjectEuler.CommandLine.CmdRunAll
  ( cmdRunAll
  ) where

import Control.Monad

import qualified Data.IntMap.Strict as IM

import ProjectEuler.AllProblems
import ProjectEuler.CommandLine.CmdRun

cmdRunAll :: [String] -> IO ()
cmdRunAll _ =
  forM_ (IM.toAscList allProblems) $ \(_,p) -> evalProblem p
