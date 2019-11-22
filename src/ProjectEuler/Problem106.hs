module ProjectEuler.Problem106
  ( problem
  ) where

import Control.Monad
import Data.Monoid
import Petbox

import qualified Data.List.Ordered as LOrdered

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 106 Solved result

{-
  Looks like we should expect this to be a difficult one.
  But I blame the problem description of not doing a good job of explaining what's going on.

  We first need to figure out where do these numbers come from:

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

  So far we know:

  - For any n, we only need to check for subsets of the same size,
    because from property 2, we can assume this is always true.

  - Subsets of size 1 can be ignored, knowing all elements are unique.

  - Subset sizes can only be <= floor(n/2),
    so we want to do this for subset size m=2,3,4,...,floor(n/2)
    for each of those pairs, if we can prove the inequality from
    sequence of elements (e.g. using A < B < C < D for n = 4),
    we don't need to check it.

  Taking this into account, there's only 3 pairs remaining:

  - {A,B}, {C,D}: for this one, we know A+B < C+D, no need of checking
    (or, in other words, this is because A < C and B < D)
  - {A,C}, {B,D}: for this one, A<B, C<D, no need of checking
  - {A,D}, {B,C}: looks like this is the remaining one.

  Notice the pattern: if we can pair all elements (taking from two sets)
  in both set using the inequation A < B < C < D, then the comparison
  is not needed.

  Well, we need to investigate n > 4 and see if we can find a pattern.
  More precisely, let the full set contain n elements, and
  let's assume we are pairing two disjoint set of the same size m,
  we want to investigate the number of pairs that we cannot draw a conclusion
  by simply looking at `A < B < C < ... < ...`.

  Experiment:
  - Given 0 .. n, create pairs of disjoint sets of the same size m
  - see if we can avoid equality test by just discharging number pairs (a,b),
    whenever a < b.
  - print out / count remainings

  So, turns out testing on 12 is fast enough to solve the problem.

 -}

pickSomeInOrder :: Int -> [a] -> [[a]]
pickSomeInOrder 0 _ = [[]]
pickSomeInOrder n xs = do
  (y,ys) <- pickInOrder xs
  (y:) <$> pickSomeInOrder (n-1) ys

{-
  generate pairs of disjoint sets,
  from [1..n], with each set has m elements.
 -}
disjointPairs :: Int -> Int -> [] ([Int], [Int])
disjointPairs n m = do
    l <- sets
    r <- sets
    guard $ l < r
    guard $ null (LOrdered.isect l r)
    pure (l,r)
  where
    sets = pickSomeInOrder m [1..n]

{-
  given two disjoint subset (sorted),
  see if we can check for its inequality
  by pairing all elements (from both sets)
  using inequality.
 -}
_alreadyInequal :: ([Int], [Int]) -> Bool
_alreadyInequal ([],[]) = True
_alreadyInequal (xs,ys) = or $ do
  (x,xs') <- pick xs
  (y,ys') <- pick ys
  guard $ x < y
  pure $ _alreadyInequal (xs',ys')

{-
  `needEqualTest 12` also computes the final answer.
  slow, as this is brute force.
 -}
_needEqualTest :: Int -> Int
_needEqualTest n = sum $ do
  m <- [2..quot n 2]
  let count = filter (not . _alreadyInequal) $ disjointPairs n m
  pure (length count)

{-
  needEqualTest <$> [4..10]
  > 1,2,3,6,11,23,47,106,235
  oeis gives: https://oeis.org/A304011
 -}

result :: Int
result = getSum $ foldMap countPairs [2..quot n 2]
  where
    n = 12
    countPairs i = Sum $ choose n (i+i) * choose (i+i-1) (i-2)
