module ProjectEuler.Problem110
  ( problem
  , pickInOrder'
  ) where

import Control.Arrow
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

search :: [Int] -> Int -> [] (Int, [Int])
search odds acc = do
  (x,odds') <- pickInOrder' odds
  let acc' = acc*x
  if acc' >= minCount * 2
    then pure (acc', [x])
    else second (x:) <$> search odds' acc'

-- this finds a working solution but not necessarily the minimum solution.
result = take 5 $ search [3,5..] 1


