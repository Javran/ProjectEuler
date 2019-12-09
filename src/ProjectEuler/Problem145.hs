module ProjectEuler.Problem145
  ( problem
  ) where

import Petbox
import Data.Monoid

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 145 Solved result

{-
  Idea:

  A brute force gets the answer.

  Afterthoughts: turns out there is no 9-digit solutions,
  which allows us to simply search 1~10^8-1 to get the right answer.

  Certainly we can do better than this: let n be the number of digits of that number in question.

  First notice that if the most (or, eqivalently, least) significant pair of digits does not carry,
  no any other pair of digits can.

  Say the number is [abcdef], and reverse [fedcba]:

   [abcdef]
  +[fedcba]

  let odd number a + f < 10 so it won't carry. for this number to be reversible, we want e + b to be odd.
  observe the most significant pair of digits, since a + f is alread odd, we don't want b + e carry.
  and this reasoning works for all cases: if the most / least significant pair of digits does not carry,
  none of the other parts will.

  When n is even, we expect no pair of digits to carry, otherwise it introduces an inconsistency:
  For example, say the number is [abcd].
   [abcd]
  +[dcba]
  if a + d is odd and carries, c + b must be even, but since (b,c) is next to (c,b), c+b should carry,
  which will make a+d an even number.

  (TODO: work out the formula)

  Then we can realize, when n is odd, carrying must happen for the number to be reversible,
  say the number is [abc]:

   [abc]
  +[cba]

  The middle digit can only be even if no carrying can happen.

 -}

isReversible :: Int -> Bool
isReversible x = x `rem` 10 /= 0 && all odd xs
  where
    xs = intToDigitsRev (x + numReverseInBase 10 x)

result :: Int
result =
  getSum
  . foldMap (\x -> if isReversible x then 1 else 0) $
    [1..10^!8-1]
