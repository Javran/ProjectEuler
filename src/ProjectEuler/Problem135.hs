module ProjectEuler.Problem135
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 135 Unsolved result

{-
  Let's get some symbol pushing going:

  (m+d)^2 - m^2 - (m-d)^2 = n > 0.

  w.l.o.g.: m > d > 0.
  this also implies that m > 1, we don't have a value for d otherwise.

  (m+d)^2 - m^2 - (m-d)^2
  = (m+d+m-d)(m+d-m+d) - m^2
  = 4 m d - m^2
  = (4 d - m) * m = n > 0

  so this gives us another constraint:

  4 d - m > 0 => m < 4 d,

  so for a fixed value n, m can be one of its divisor.

  d = (n + m^2) / (4 * m) must be an integer.

  If we further plug in d < m:

  (n + m^2) / 4 * m < m
  >  n < 3 * m^2

  This gives us a tighter range on m.
  Let's try this idea out.

 -}

result = ()

