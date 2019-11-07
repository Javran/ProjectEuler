module ProjectEuler.Problem127
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 127 Unsolved result

{-
  No idea at first, as always. But there are few things that might come in handy:

  - note that a + b = c, so we only need to search for two numbers and the third one
    can be derived from that.
  - given that a, b, c are pair-wise coprimes, therefore
    rad(a * b * c) = rad(a) * rad(b) * rad(c), by definition (no shared prime factor).


 -}

result = ()


