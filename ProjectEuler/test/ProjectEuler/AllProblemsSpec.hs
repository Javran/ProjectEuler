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

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Control.Monad.IO.Class
import Control.Applicative
import Data.Coerce
import TextShow
import Control.Exception

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
                  {-
                    First interpret the field as Text,
                    in case of failure, try Integer and convert it to Text.

                    this allows writing:

                    > - 12345

                    instead of the verbose version:

                    > - "12345"

                   -}
                  convertLine v' =
                    withText "OutputLine" pure v'
                    <|> withScientific
                          "OutputLine"
                          (pure . showt @Integer . round)
                          v'
          ys <- withArray "AnswerList" convertAnswerOuts xs
          pure (v, ys)

expectedAnswers :: Answers
expectedAnswers = case Yaml.decodeEither' $ getDataRawContent "answers.yaml" of
  Left e -> error (displayException e)
  Right a -> a

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
              case IM.lookup pId . coerce $ expectedAnswers of
                Nothing ->
                  pendingWith "Missing test case."
                Just expectedOuts -> do
                  ((), outs) <- liftIO $ runPEM pRun
                  outs `shouldBe` expectedOuts
