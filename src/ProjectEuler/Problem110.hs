module ProjectEuler.Problem110
  ( problem
  , pickInOrder'
  ) where

import Control.Arrow
import Data.List
import Petbox

import ProjectEuler.Types

import Debug.Trace

{-
  This is basically the more difficult version of Problem108,
  my plan is to revisit that problem first and come back
  with perhaps some more insights.

  By the same insight we get from Problem108,

  we know `product (take 14 primes) = 13082761331670030`
  is a solution, but now problem lies in how to narrow this down:
  so far we are trying to reach 4000000 * 2 through powers of 3,
  but actually we should take product of numbers of odd numbers greater than 1,
  [3,5,7...] and see which of those are minimal.

 -}

problem :: Problem
problem = pureProblem 110 Unsolved result

minCount = 4000000

-- TODO: this is the same function being used in Problem109,
-- might worth make it into SolCommon
pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' x = (\(u,v) -> (u,u:v)) <$> pickInOrder x

search :: Int -> [Int] -> Int -> [] (Int, [Int])
search upBnd odds acc = do
  (x,odds') <- pickInOrder' $ takeWhile (\x -> x *acc < upBnd) odds
  let acc' = acc*x
  if acc' >= minCount*2
    then
      if acc' < upBnd
        then pure (acc', [x])
        else []
    else
      second (x:) <$> search upBnd odds' acc'

{-
  this finds a working solution but not necessarily the minimum solution.
  update: found (take 14) by trial and error,
  this give us: (8000001,[3,3,67,13267]) -- doesn't feel right to me
  as the last one is a bit too large.
 -}
result = unfoldr improve (3 ^! 15 + 1)
  where
    improve :: Int -> Maybe ((Int, [Int]), Int)
    improve upBnd = do
      (h@(b',_), _tl) <- uncons $ search upBnd [3,5..] 1
      pure (h, b')

--   head $ search (3 ^! 15 + 1) [3,5..] 1


