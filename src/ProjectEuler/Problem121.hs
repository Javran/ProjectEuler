module ProjectEuler.Problem121
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 121 Unsolved result

{-
  First thing, let's work out these numbers:

  - Why prize fund is at most 10?

    Note that for game that has p / q probability to win,
    Let x be the prize fund,
    We earn 1*q for each game played, and loss x*p when
    player wins the game.
    Therefore we have 1*q > x*p,
    so x is maximum integer such that x < q / p.
    Indeed for p / q = 11 / 120, x < 120 / 11, and we can at most have x = 10.

  - Why 11 / 120?

    Denominator is easy to explain, the probability of taking a blue disc is:

    + 1/2 for 1st round
    + 1/3 for 2nd round
    + 1/4 for 3rd round
    + 1/5 for 4nd round

    and 120=2*3*4*5.

    As for numerator, let's represent a game by disc taken
    in sequence, so all winning plays are:

    + b,b,b,b: 1 case
    + r,b,b,b: 1 case
    + b,r,b,b: 2 cases
    + b,b,r,b: 3 cases
    + b,b,b,r: 4 cases

  Actually have already found the pattern: for a game that plays n rounds,
  We need strictly more than n/2 rounds of picking blue disc,
  which is equivalent to say it's less than n/2 rounds of picking red disc.

  - there is one case where we pick blue disc every time.
  - there are 1 + 2 + ... + n cases where exactly 1 red disc is picked.
  - there are [1*n + 2*n + ... + (n-1)*n] + [1*(n-1) + 2*(n-1) + ... + (n-2)*(n-1)] ...

    or in other words, if there are exactly m red disc being picked,
    we are actually count all the cases of picking m values from 1,2,3,...,n without replacement
    and taking the product of these m numbers. - I'm interested to see whether there's some
    existing formula about how to do this efficiently.

 -}

{-
  `sumProd n m` computes the sum of products formed by taking
  exactly m numbers from 1,2,3,...n without replacement.
 -}
sumProd :: Int -> Int -> Int
sumProd n m = sum $ aux 1 m [1..n]
  where
    aux acc 0 _ = pure acc
    aux acc t candidates = do
      (c, candidates') <- pickInOrder candidates
      let acc' = c * acc
      acc' `seq` aux acc' (t-1) candidates'

-- This search finds us https://oeis.org/A094638.
result = sumProd 6 <$> [0..6]

