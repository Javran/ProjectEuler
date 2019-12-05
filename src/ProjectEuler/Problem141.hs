module ProjectEuler.Problem141
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 141 Unsolved result

{-
  Looks like this will be one of those difficult ones.

  Say n = q * k + r.

  - First, we don't want any of those to be zero,
    since we want q,k,r to form geometric sequence (not in that particular order).
    Therefore: q > 0 & k > 0 & r > 0
  - Also 0 < r < q

  So r < q is already in place, leaving k only 3 choices:
  - k < r < q, in this case we can verify whether k * q == r * r
  - r < k < q, verify that r * q == k * k
  - r < q < k, verify that r * k == q * q

  Perhaps explore a bit more with each cases?

  - case #1: k < r < q.
    let r = k * z, q = k * z * z
    n = q * k + r = k * z * z * k + k * z = k z (k z + 1) = x^2 (TODO)

  - case #2: r < k < q.
    let k = r * z, q = r * z * z
    n = q * k + r = r * z * z * r * z + r
      = r (z^3 r + 1) = x^2 (TODO)

  - case #3: r < q < k.
    let q = r * z, k = r * z * z
    n = q * k + r = r * z * r * z * z + r
      = r (z^3 r + 1) = x^2 (TODO)
    For this case we end up having the same equation as case #2 to deal with.

 -}

result = ()

