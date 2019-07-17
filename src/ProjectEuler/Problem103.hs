module ProjectEuler.Problem103
  ( problem
  ) where

import ProjectEuler.Types
import ProjectEuler.Problem105 (findSpecialSubsets)

problem :: Problem
problem = pureProblem 103 Solved result

{-
  First guess: looking at last number of each optimum set:

  > 1,2,4,7,13,25

  From the looks of it we may want to assume the growth is slightly
  less than 2 at each step, so let's assume maximum number is 50 and
  walk our way down.

 -}

{-
  Note 1: but listing candidates as [50,49...],
  we managed to get one solution: [26,37,43,46,48,49,50],
  so at least we have found an upper bound of the final solution.

  Note 2: through trial-and-error (if program doesn't give an answer
  in few seconds, perhaps we don't really have a solution),
  we find that [46,45...] gives [22,33,39,42,44,45,46],
  while [45,44..] take forever - I'd say that's a closer upperbound.

  Note 3: now let's turn it around and use candidate list [1..46].
  The reason for this is that:
  we already found a close upperbound for the max number,
  and want to find the first solution, which got to be the optimum.

  Note 4: `findSpecialSubsets` is originally implemented in this module
  and then moved to Problem105, because the function in Problem105
  is more general than the version that was here.
 -}

result :: Int
result =
  read . concatMap show . head
  $ findSpecialSubsets 7 [1..46]
