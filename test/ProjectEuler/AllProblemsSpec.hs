module ProjectEuler.AllProblemsSpec where

import Control.Monad
import Control.Monad.IO.Class
import Test.Hspec

import qualified Data.IntMap.Strict as IM

import ProjectEuler.AllProblems
import ProjectEuler.GetData
import ProjectEuler.Types

spec :: Spec
spec =
  describe "allProblems" $
    forM_ (IM.toAscList allProblems) $
      \(_, Problem pId pSt pRun) ->
        specify ("Problem #" <> show pId) $
          case pSt of
            Unsolved ->
              pendingWith "This problem is not yet solved."
            Solved ->
              case getExpectedAnswers pId of
                Nothing ->
                  pendingWith "Missing test case."
                Just expectedOuts -> do
                  ((), outs) <- liftIO $ runPEM pRun
                  outs `shouldBe` expectedOuts
