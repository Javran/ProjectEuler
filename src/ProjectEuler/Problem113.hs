module ProjectEuler.Problem113
  ( problem
  ) where

import ProjectEuler.Types

{-
  Idea: could try dynamic programming for this one.

  Let f(l,d) be the number of non-decreasing number of length l
  whose last digit is d (leading zeros are forbidden),
  this will allow us to write f(l,d) in terms of f(l-1,d') in which d' <= d,
  then a similar formula can be written down for non-increasing numbers.
  putting these two together, we'll have the number of non-bonucy numbers
  below 10^100 (namely those that has <= 100 digits)

  let's define f(l,d) properly (d <- [1..9])

  f(1,0) = 0
  f(1,_) = 1

  f(l,d) = sum of f(l-1,d') where 0 <= d' <= d

  TODO: this can be sped up by deriving f(l,d) from f(l-1,d) and f(l,d-1)

  now let g(l,d) be the number of non-increasing number of length l.

  g(1,0) = 0
  g(1,_) = 1
  g(l,d) = sum of g(l-1,d') where d <= d' <= 9

  let h(l) be the number of non-bouncy numbers of length l:

  h(l) = sum {f(l,d) + g(l,d)} - t(l) for d <- [0..9]

  where t(l) is the number of number that are both non-increasing and non-decreasing
  of length l:

  t(_) = 9

 -}

problem :: Problem
problem = pureProblem 113 Unsolved result

f :: Int -> Int -> Int
f 1 0 = 0
f 1 _ = 1
f l d = sum [f (l-1) d' | d' <- [0..d]]

g :: Int -> Int -> Int
g 1 0 = 0
g 1 _ = 1
g l d = sum [g (l-1) d' | d' <- [d..9]]

h :: Int -> Int
h l = sum [f l d + g l d | d <- [0..9]] - 9

{-

  Update: the proof of concept works for 6 digits and 10 digits:

  > sum [h l | l <- [1..6]]
  12951
  > sum [h l | l <- [1..10]]
  277032

 -}

result :: Int
result = sum [h l | l <- [1..10]]

