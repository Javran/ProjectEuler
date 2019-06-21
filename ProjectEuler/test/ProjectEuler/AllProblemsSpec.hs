{-# LANGUAGE OverloadedStrings #-}
module ProjectEuler.AllProblemsSpec where

import Test.Hspec
import Control.Monad

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import ProjectEuler.AllProblems
import ProjectEuler.GetData
import ProjectEuler.Types
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable

newtype Answers = Answers (IM.IntMap [T.Text])

instance FromJSON Answers where
  parseJSON =
    withObject "Answers" $ \v ->
        Answers . IM.fromList <$> mapM convert (HM.toList v)
      where
        convert :: (T.Text, Value) -> Parser (Int, [T.Text])
        convert (t, xs) = do
          [(v, "")] <- pure $ reads (T.unpack t)
          let convertAnswerOuts :: Array -> Parser [T.Text]
              convertAnswerOuts = mapM (withText "OutputLine" pure) . toList
          ys <- withArray "AnswerList" convertAnswerOuts xs
          pure (v, ys)

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
              pending
