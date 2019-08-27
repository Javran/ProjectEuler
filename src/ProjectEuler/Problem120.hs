module ProjectEuler.Problem120
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 120 Solved result

{-
  Idea:

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
f a n =
  if even n
    then 2
    else (2 * n * a) `rem` (a * a)

findMax :: Int -> Int
findMax a = maximum $ 2 : [f a n | n <- [1,3..a*2-1]]

result :: Int
result = sum $ findMax <$> [3..1000]
