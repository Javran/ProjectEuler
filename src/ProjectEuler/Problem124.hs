module ProjectEuler.Problem124
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 124 Unsolved result

{-
  Idea: instead of enumerating all numbers in range
  and produce their radicals, I plan to:

  - generate all radicals within range first
  - then for each radical, generate numbers of that radical
    (so we basically need the radical number and the set of primes)
  - note that in the sorted results, numbers from same radical
    is always groupped together, we can take advantage of this property
    to skip through elements.
 -}

result = ()


