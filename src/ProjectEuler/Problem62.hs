module ProjectEuler.Problem62
  ( problem
  ) where

import Control.Arrow
import Data.Function
import Data.List
import Data.Tuple
import Petbox

import qualified Data.Map.Strict as M

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 62 Solved result

-- get a list of (num, num^3 (broken into digits)),
-- and then group by cubic number length
sameCubicLengthGroups :: [] [(Int,[Int])]
sameCubicLengthGroups =
  groupBy ((==) `on` (length . snd))
  . map (id &&& (intToDigits . (^(3 :: Int))))
  $ [1..]

permSet :: [(Int,[Int])] -> [ [Int] ]
permSet =
  M.elems
  . M.fromListWith (++)
  . map (swap . second sort . first (:[]))

result :: Int
result = minimum $ map (^(3 :: Int)) answerGroup
  where
    cubicPermSets = map permSet sameCubicLengthGroups
    -- this is the permset we interested in:
    -- a group containing exactly 5 elements
    answerGroup:_ = concatMap (filter ((== 5) . length)) cubicPermSets
