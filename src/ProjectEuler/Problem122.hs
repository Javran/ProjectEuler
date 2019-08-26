{-# LANGUAGE TupleSections #-}
module ProjectEuler.Problem122
  ( problem
  ) where

import Control.Monad

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import ProjectEuler.Types
import ProjectEuler.SolCommon

{-
  Idea:

  Related: https://en.wikipedia.org/wiki/Addition-chain_exponentiation

  The problem is the same as finding the fasted way to add up to certain number
  starting with:

  - `1` as the only known number.
  - `+` as the only allow operation.

  It seems that there's no efficient algorithm for this,
  I imagine BFS is the best we can do.

  The search state can be represented as two things:

  - an IntSet, representing all numbers we have computed so far.
  - a integer representing the total number of operations to reach that IntSet.

  In addition, since we need to search the best method for k <- [1..200],
  we can do some sharing and finish these tasks in one go:

  - We begin with initial state {1}
  - For each step, take two elements (with replacement) from the set, do addition,
    and put the resulting element into set (fail if the new element already exists)
  - We also maintain another set, initialized as [1..200], for each layer of operation,
    we can "discharge" some numbers from this set when they can be reached
    for the first time in this layer (by "can be reached for the first time" I meant
    that exact value can be generated in this layer but not in any earlier layers)
    and record somewhere about which layer a particular number is reached
    (this layer number is also the number of operations to reach that number)
  - looks like a straightforward non-deterministic computation should do the trick,
    the algorithm stops when all values can be reached, at which point
    we can just sum up to reach the final answer.
 -}

problem :: Problem
problem = pureProblem 122 Solved result

{-
  perform one operation non-deterministically,
  return the number being produced and the resulting set.
  (the produced number must be new)
 -}
nextOp :: IS.IntSet -> [] (Int, IS.IntSet)
nextOp s = do
  let xs = IS.toAscList s
  {-
    since addition is commutative,
    there is no need to check for b + a
    if we already attempted a + b.
   -}
  (a,xs') <- pickInOrder' xs
  (b,_) <- pickInOrder' xs'
  let c = a + b
  {-
    We enforce that we only produce values that are greater
    than any that we already have - since other states
    in the same layer should have those "smaller" cases covered anyway.
   -}
  guard $ IS.findMax s < c
  pure (c, IS.insert c s)

solveAll :: IS.IntSet -> IM.IntMap Int -> Int -> S.Set IS.IntSet -> IM.IntMap Int
solveAll todoSet results opCnt states
  | IS.null todoSet = results
  | otherwise =
      let nextStates =
            {-
              only examine those that produces missing numbers.
              this filter is more of a heuristic, but it helps us to
              dramatically reduce number of states,
              and even if we get a wrong answer, we can consider this as the upperbound.
              Update: turns out we do produce the right answer using this.
             -}
            filter ((`IS.member` todoSet) . fst) $
              S.toList states >>= nextOp
          states' = snd <$> nextStates
          newlyNums = fst <$> nextStates
          opCnt' = opCnt+1
          results' = IM.union results (IM.fromList $ (,opCnt') <$> newlyNums)
          todoSet' = IS.difference todoSet (IS.fromList newlyNums)
      in solveAll todoSet' results' opCnt' (S.fromList states')

result :: Int
result =
  sum
  . fmap snd
  . IM.toList
  $ solveAll
    -- note that 1 -> 0, we don't really need to count k = 1 case anyway.
    (IS.fromDistinctAscList [2..200]) IM.empty 0 $
    S.singleton (IS.singleton 1)

