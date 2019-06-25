module ProjectEuler.Problem39
  ( problem
  ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Monoid
import Data.Ord

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 39 Solved result

solutions :: Int -> Int
solutions p = getSum $ foldMap (const 1) sol
  where
    sol = do
      a <- [1..p]
      b <- [a..p]
      let c = p - a - b
      guard $ c*c == a*a + b*b
      pure (a,b,c)

result :: Int
result =
  -- TODO: maximumOn?
  fst $ maximumBy (comparing snd) $ (id &&& solutions) <$> [1..1000]

