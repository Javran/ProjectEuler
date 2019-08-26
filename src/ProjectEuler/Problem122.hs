module ProjectEuler.Problem122
  ( problem
  ) where

import ProjectEuler.Types

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
problem = pureProblem 122 Unsolved result

result = ()


