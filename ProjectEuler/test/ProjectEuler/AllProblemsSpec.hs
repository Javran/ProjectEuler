{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  #-}
module ProjectEuler.AllProblemsSpec where

import Test.Hspec
import Control.Monad

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml

import System.FilePath
import Data.Aeson
import Data.Maybe
import Data.Aeson.Types
import Data.Foldable
import Control.Monad.IO.Class
import Control.Applicative
import Data.Coerce
import TextShow

import ProjectEuler.AllProblems
import ProjectEuler.GetData
import ProjectEuler.Types

newtype Answers = Answers (IM.IntMap [T.Text]) deriving Show

instance FromJSON Answers where
  parseJSON =
    withObject "Answers" $ \v -> do
        obj <- v .: "answers"
        Answers . IM.fromList <$> mapM convert (HM.toList obj)
      where
        convert :: (T.Text, Value) -> Parser (Int, [T.Text])
        convert (t, xs) = do
          [(v, "")] <- pure $ reads (T.unpack t)
          let convertAnswerOuts :: Array -> Parser [T.Text]
              convertAnswerOuts = mapM convertLine . toList
                where
                  convertLine v' =
                    withText "OutputLine" pure v'
                    <|> withScientific "OutputLine" (pure . showt @Integer . round) v'
          ys <- withArray "AnswerList" convertAnswerOuts xs
          pure (v, ys)

getExpectedAnswers :: IO Answers
getExpectedAnswers = do
  dir <- getDataDir
  fromJust <$> Yaml.decodeFileThrow (dir </> "data" </> "answers.yaml")

spec :: Spec
spec =
  describe "allProblems" $
    forM_ (IM.toAscList allProblems) $
      \(_, Problem pId pSt pRun) ->
        specify ("Problem #" <> show pId) $
          case pSt of
            Unsolved ->
              pendingWith "This problem is not yet solved."
            Solved -> do
              -- TODO: it is wasteful to do "getExpectedAnswers" every time,
              -- consider file-embed to get rid of runtime loading.
              r :: Maybe [T.Text] <- IM.lookup pId . coerce <$> liftIO getExpectedAnswers
              case r of
                Nothing ->
                  pendingWith "Missing test case."
                Just expectedOuts -> do
                  ((), outs) <- liftIO $ runPEM pRun
                  outs `shouldBe` expectedOuts
