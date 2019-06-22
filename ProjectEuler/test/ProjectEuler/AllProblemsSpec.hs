{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  #-}
module ProjectEuler.AllProblemsSpec where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Data.Foldable
import Data.Scientific
import Test.Hspec
import TextShow

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

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
                  scientificToInteger :: Scientific -> Parser Integer
                  scientificToInteger s = do
                    Right i <- pure (floatingOrInteger @Double s)
                    pure i

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
                          ((showt @Integer <$>) . scientificToInteger)
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
