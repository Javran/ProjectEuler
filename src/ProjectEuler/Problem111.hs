module ProjectEuler.Problem111
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 111 Unsolved result

{-
  There's about ~400000000 prime numbers that has 10 digits,
  checking all of them will take too long.

  First step: we can begin with figuring out M(10,d) for d = 0..9,
  the idea is to generate numbers with 10 repeated number, 9 repeated number, etc.
  until we find one prime, at which point we'll known M(10,d) for
  that particular d. After this step is done,
  we can proceed to generate numbers with M(10,d) repeated numbers,
  test again, to eventually get all primes of that pattern and
  therefore obtain S(10,d).
 -}

result = ()
