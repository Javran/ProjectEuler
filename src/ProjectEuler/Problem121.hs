module ProjectEuler.Problem121
  ( problem
  ) where

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

 -}

result = ()


