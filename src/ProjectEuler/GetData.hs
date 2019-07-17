{-# LANGUAGE
    TemplateHaskell
  , TypeApplications
  , OverloadedStrings
  #-}
module ProjectEuler.GetData
  ( getDataRawContent
  , getDataContent
  , expectedAnswers
  , getExpectedAnswers
  , pureProblemWithData
    {-
      re-export types so that all problems that read data
      won't need to do 2 module imports.
     -}
  , module ProjectEuler.Types
  ) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Data.FileEmbed
import Data.Foldable
import Data.Maybe
import Data.Scientific
import Data.Text.Encoding (decodeUtf8)
import TextShow

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import ProjectEuler.Types

{-
  Data files are constructed at compile time to reduce runtime overhead.

  Here we provides 2 versions of the same file:
  - ByteString version, if needed for parsing.
  - Text version, this will cover most use cases.
 -}
dataDirContents :: M.Map FilePath (BS.ByteString, T.Text)
dataDirContents = M.fromList $ second (\x -> (x, decodeUtf8 x)) <$> $(embedDir "data")

getDataPair :: FilePath -> (BS.ByteString, T.Text)
getDataPair p =
    {-
      The error raise is intentional because `data/` path
      is under version control and shouldn't contain any files
      that are unknown statically.
     -}
    fromMaybe err $ M.lookup p dataDirContents
  where
    err = error $ "Data file not found: " <> p

getDataRawContent :: FilePath -> BS.ByteString
getDataRawContent = fst . getDataPair

getDataContent :: FilePath -> T.Text
getDataContent = snd . getDataPair

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

expectedAnswers :: IM.IntMap [T.Text]
expectedAnswers = case Yaml.decodeEither' $ getDataRawContent "answers.yaml" of
  Left e -> error (displayException e)
  Right a -> coerce @Answers a

getExpectedAnswers :: Int -> Maybe [T.Text]
getExpectedAnswers pId = IM.lookup pId expectedAnswers

-- Like pureProblem but in addition allows specifying a data file,
-- which will be loaded as argument to the function that computes the solution.
pureProblemWithData :: TextShow r
                    => String -> Int -> ProblemStatus -> (T.Text -> r) -> Problem
pureProblemWithData dFile pId pSt runWithData = Problem pId pSt $
  logT $ runWithData (getDataContent dFile)

{-

  The following part is updated by `pet sync` to describe
  the full list of data files compiled into the binary.
  Content update to this file, even if it's just update on commented parts,
  is enough to trick the building system into rebuilding this module.

  Note that the following section is padded with some spaces ("  #")
  instead of directly starting with "#", this is intentional,
  as some editor might otherwise recognize them as macro-related stuff.

  # ==== DATA_FILE_LIST_BEGIN
  - AllProblems.hs.mustache
  - ProblemX.hs.mustache
  - answers.yaml
  - p008_product.txt
  - p011_grid.txt
  - p013_numbers.txt
  - p018_path.txt
  - p022_names.txt
  - p042_words.txt
  - p054_poker.txt
  - p059_cipher.txt
  - p067_triangle.txt
  - p079_keylog.txt
  - p081_matrix.txt
  - p082_matrix.txt
  - p083_matrix.txt
  - p089_roman.txt
  - p096_sudoku.txt
  - p098_words.txt
  - p099_base_exp.txt
  - p102_triangles.txt
  - p105_sets.txt
  # ==== DATA_FILE_LIST_END

 -}
