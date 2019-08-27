module ProjectEuler.Problem120
  ( problem
  ) where

import Math.NumberTheory.Powers.Modular
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 120 Solved result

{-
  Idea: not sure what to do yet, see if I can find some pattern:

  > f 6 <$> [0..20] -- 6
  [2,12,2,0,2,24,2,12,2,0,2,24,2,12,2,0,2,24,2,12,2]

  > f 7 <$> [0..20]
  [2,14,2,42,2,21,2,0,2,28,2,7,2,35,2,14,2,42,2,21,2] -- 14

  > f 8 <$> [0..20] -- 2
  [2,16,2,48,2,16,2,48,2,16,2,48,2,16,2,48,2,16,2,48,2]

  > f 11 <$> [0 .. 40] -- 22
  [2,22,2,66,2,110,2,33,2,77,2,0,2,44,2,88,2,11,2,55,2,99,2,22,2,66,2,110,2,33,2,77,2,0,2,44,2,88,2,11,2]

  > f 26 <$> [0..40] -- 26
  [2,52,2,156,2,260,2,364,2,468,2,572,2,0,2,104,2,208,2,312,2,416,2,520,2,624,2,52,2,156,2,260,2,364,2,468,2,572,2,0,2]

  Note that:
  - when n is even, the result is always 2.
  - 2, n*2, 2, n*2*3 mod n*n, 2, n*2*3*6 mod n*n, 2, ...
  - every number seems to eventually run into a circle.

  For now it looks like for any n, the max r can be found by trying
  just n <- [0..a*2-1].


  Some more insights on this one:

  let B(n,k) denote "n choose k" (a.k.a binomial coefficient)

  (a + 1)^n = sum{ B(n,i)*a^(n-i)*1^i, i = 0 to n }
  (a - 1)^n = sum{ B(n,i)*a^(n-i)*(-1)^i, i = 0 to n }

  notice that these two expansion has same number of terms, and when i is odd,
  some terms will cancel each other, leaving:

  (a + 1)^n + (a - 1)^n = 2 * sum{ B(n,i)*a^(n-i), i = 0 to n && i is even }

  note that a^k mod a^2 = 0 for all k >= 2, therefore:

  [(a + 1)^n + (a - 1)^n] (mod a^2)
  = 2 * sum{ B(n,i)*a^(n-i), i = 0 to n && i is even } (mod a^2)
  = 2 * sum{ B(n,i)*a^(n-i), i = 0 to n && i is even && n - i < 2} (mod a^2)
  (n - i < 2 restricts i to only 2 possible values: i = n or i = n - 1.)
  = 2 * sum{ B(n,i)*a^(n-i), i = n-1 or n && i is even } (mod a^2)

  when n is even:
  [(a + 1)^n + (a - 1)^n] (mod a^2)
  = 2 * sum{ B(n,i)*a^(n-i), i = n } (mod a^2)
  = 2 * B(n,n) (mod a^2)
  = 2 (mod a^2)

  when n is odd:
  [(a + 1)^n + (a - 1)^n] (mod a^2)
  = 2 * sum{ B(n,i)*a^(n-i), i = n-1 } (mod a^2)
  = 2 * B(n,n-1) * a (mod a^2)
  = 2 * n * a (mod a^2)

 -}

f :: Int -> Int -> Int
f a n = (powModInt (a-1) n aSq + powModInt (a+1) n aSq) `rem` aSq
  where
    aSq = a * a

findMax :: Int -> Int
findMax a = maximum $ 2 : [f a n | n <- [1,3..a*2-1]]

{- TODO: We are kind of brute forcing this, I feel there should be a better way -}
result :: Int
result = sum $ findMax <$> [3..1000]
