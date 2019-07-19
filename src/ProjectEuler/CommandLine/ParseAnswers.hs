module ProjectEuler.CommandLine.ParseAnswers
  ( parseAnswersSection
  ) where

import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

{-
  This module parses "answers" field of data/answers.yaml.

  We basicaly just need to handle two formats:

  > <problem id>: <newline>
  >   - <line 1>
  >   - <line 2>
  >   - ...

  or:

  > <problem id>: <content> <newline>

  For all output lines, spaces are stripped from beginnings and ends.

 -}

skipNonNewlineSpaces :: ReadP ()
skipNonNewlineSpaces =
  void $ munch (\c -> c /= '\n' && isSpace c)

problemHeaderP :: ReadP Int
problemHeaderP =
  read <$>
    munch1 isDigit
    <* char ':'
    <* skipNonNewlineSpaces

singleLineContentP :: ReadP String
singleLineContentP =
  munch1 (not . isSpace)
    <* skipNonNewlineSpaces
    <* char '\n'

multiLineContentsP :: ReadP [String]
multiLineContentsP = many $
  string "  -"
    *> skipNonNewlineSpaces
    *> singleLineContentP

problemP :: ReadP (Int, [String])
problemP = do
  pId <- problemHeaderP
  outs <-
    ((:[]) <$> singleLineContentP)
    <++ (char '\n' *> multiLineContentsP)
  pure (pId, outs)

parseAnswersSection :: String -> [(Int, [String])]
parseAnswersSection raw = case readP_to_S (many problemP <* eof) raw of
  [(r, "")] -> r
  _ -> error "unexpected format."
