module ProjectEuler.Problem106
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 106 Unsolved result

{-
  Looks like we should expect this to be a difficult one.

  Taking some early notes: we first need to figure out
  how the heck do these numbers come from:

  - n = 4, 1 out of 25 subsets pairs needs to be tested.
  - n = 7, 70 out of 966 subset pairs needs to be tested.
  - n = 12, <?> out of 261625 subset pairs needs to be tested.

  Now that "261625" looks like a distinct number that we can search.
  Indeed, a OEIS search on "25" "966" "261625" yields: https://oeis.org/A000392
  see if we can make sense of how this fits into what we want to solve.

  Another note: no need to check for the second property suggests
  that the comparison should only be performed between sum of subsets
  of same # of elements.

 -}

result :: ()
result = ()
