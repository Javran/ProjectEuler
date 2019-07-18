module ProjectEuler.Problem106
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 106 Unsolved result

{-
  Looks like we should expect this to be a difficult one.

  We first need to figure out
  where do these numbers come from:

  - n = 4, 1 out of 25 subsets pairs needs to be tested.
  - n = 7, 70 out of 966 subset pairs needs to be tested.
  - n = 12, <?> out of 261625 subset pairs needs to be tested.

  Now that "261625" looks like a distinct number that we can search.
  Indeed, a OEIS search on "25" "966" "261625" yields: https://oeis.org/A000392.

  Quote:
  > Let P(A) be the power set of an n-element set A.
  > Then a(n+1) = the number of pairs of elements {x,y} of P(A)
  > for which x and y are disjoint and for which x is not a subset of y
  > and y is not a subset of x. Wieder calls these "disjoint strict 2-combinations".

  Sounds making perfect sense in our case.

  Also, no need of checking the second property suggests
  that the comparison should only be performed between sum of subsets
  of same # of elements.

  For n = 4, assume the set is {A,B,C,D} with A < B < C < D,
  the pairs are:

  LHS     | RHS(s)
  {A}     | {B,C,D} {B,C} {B,D} {C,D} {B} {C} {D} (# = 7)
  {A,B}   | {C,D} {C} {D} (# = 3)
  {A,C}   | {B,D} {B} {D} (# = 3)
  {A,D}   | {B,C} {B} {C} (# = 3)
  {A,B,C} | {D} (# = 1)
  {A,B,D} | {C} (# = 1)
  {A,C,D} | {B} (# = 1)
  {B}     | {C,D} {C} {D} (# = 3)
  {B,C}   | {D} (# = 1)
  {B,D}   | {C} (# = 1)
  {C}     | {D} (# = 1)

  This does sum up to 25.

  So we know that
  - we can skip all checks that involve singleton sets (since we know every number is different)
  - we can skip all checks that involve two sets of different size (property 2)

  Taking this into account, there's only 3 pairs remaining:

  - {A,B}, {C,D}: for this one, we know A+B < C+D, no need of checking
  - {A,C}, {B,D}: for this one, A<B, C<D, no need of checking
  - {A,D}, {B,C}: looks like this is the remaining one.

  Well, we need to investigate n>4 and see if we can find a pattern.

 -}

result :: ()
result = ()
