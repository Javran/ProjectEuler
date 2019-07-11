module ProjectEuler.Problem98
  ( problem
  ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p098_words.txt" 98 Unsolved compute

parseWords :: T.Text -> [String]
parseWords raw = read $ "[" <> T.unpack raw <> "]"

{-
  Group the list of words in terms of anagram,
  returns a list of pairs whose elements are (<set of char used>, <the group>).
  Also group that has less than 2 elements are dropped.
 -}
groupAnagrams :: [String] -> [(S.Set Char, [String])]
groupAnagrams =
    foldMap mkGroup
    . M.elems
    . M.fromListWith (<>)
    . fmap (\x -> (sort x, [x]))
  where
    mkGroup [] = []
    mkGroup [_] = []
    mkGroup xs@(h:_) = [(S.fromList h, xs)]

compute = show . groupAnagrams . parseWords
