module ProjectEuler.Problem113
  ( problem
  ) where

import Data.List
import Data.Monoid
import Control.Arrow

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

  f(_,0) = 0
  f(1,_) = 1

  f(l,d) = sum of f(l-1,d') where 0 <= d' <= d
         = f(l-1,d) + f(l,d-1)

  now let g(l,d) be the number of non-increasing number of length l.

  g(1,0) = 0
  g(1,_) = 1
  g(l,d) = sum of g(l-1,d') where d <= d' <= 9
         =
         - g(l-1,d) + g l (d+1) (when d < 9)
         - g(l-1,d) (when d == 9)

  let h(l) be the number of non-bouncy numbers of length l:

  h(l) = sum {f(l,d) + g(l,d)} - 9 for d <- [0..9]

  where `9` is the number of number that are both non-increasing and non-decreasing
  of length l.

 -}

problem :: Problem
problem = pureProblem 113 Solved result

{-

  Original implementation:

  f _ 0 = 0
  f 1 _ = 1
  f l d = f (l-1) d + f l (d-1)

  g :: Int -> Int -> Int
  g 1 0 = 0
  g 1 _ = 1
  g l d = g (l-1) d + if d == 9 then 0 else g l (d+1)

 -}

{-

  The implementation above can be further optimized into:

  f, g :: Int -> Int -> Int
  f l d = fst (combined !! (l-1)) !! d
  g l d = snd (combined !! (l-1)) !! (9-d)

 -}

{-
  Speed up calculation of f(l,d) and g(l,d) into one list.

  Turns out if we compute f layer by layer, from left to right,
  and compute g layer by layer, from right to left,
  these two computations are basically the same and only
  differ in the first layer (the first layer is a seed list)
  This allow us to combine calculation of both function
  into this one.
 -}
combined :: [([Int], [Int])]
combined = iterate (evolve *** evolve) (seed, reverse seed)
  where
    seed = take 10 (0 : repeat 1)
    evolve :: [Int] -> [Int]
    evolve = tail . scanl' (+) 0
{-# INLINE combined #-}

{-
h :: Int -> Int
h l = sum fs + sum gs - 9
  where
    (fs, gs) = combined !! (l-1)
 -}

result :: Int
result =
  -- same as `sum [h l | l <- [1..100]]`
  (getSum . foldMap (\(a,b) -> foldMap Sum a <> foldMap Sum b) $ take 100 combined) - 900

