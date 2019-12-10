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

  - When n is even, we expect no pair of digits to carry, otherwise it introduces an inconsistency:
    For example, say the number is [abcd].

     [abcd]
    +[dcba]

    if a + d is odd and carries, c + b must be even, but since (b,c) is next to (c,b), c+b should carry,
    which will make a+d an even number.

    For the most & least significant pair of digits, it can't be 0:
    > length [(a,b) | a <- [0..9], b <- [0..9], let n = a + b, odd n, n < 10, a /= 0, b /= 0]
    20

    And for those "inner" ones:
    > length [(a,b) | a <- [0..9], b <- [0..9], let n = a + b, odd n, n < 10, n /= 0]
    30

    There are in total (n/2) pairs, so the count is 20 * 30^(n/2).

  - When n is odd, carrying must happen for the middle digit in order for the number to be reversible,
    this is because, say the number is [abc]:

     [abc]
    +[cba]

    The middle digit can only be even (since it's adding two same numbers)
    if no carrying of its previous pair of digits can happen. From this we know n = 1 is impossible.

    Now since we've already looking at n=3 case, might as well look further:

    - c+a is odd, and must carry

    > length [(a,c) | a <- [1..9], c <- [1..9], let n = a + c, odd n, n >= 10]
    20

    - 2b+1 < 10 to prevent the middle digit from carrying.

    > length [ b | b <- [0..9], b + b + 1 < 10 ]
    5

    That's 20*5=100 choices in total.

    When n = 5:

     [abcde]
    +[edcba]

    We know (from least significant pair to most significant one)
    - e+a is odd and must carry
    - d+b is even and d+b+1 must carry
    - c+c+1 must carry
    However, if b+d+1 carries to the most significant digit,
    it will make that digit even - therefore this is impossible for when n = 5.

    When n = 7:

     [abcdefg]
    +[gfedcba]

    Again we know:
    - a+g is odd and must carry, a /= 0, g /= 0: 20 pairs to choose from.
    - f+b is even, and f+b+1 should not carry (otherwise we will carry 1 to the most significant a+g.

    > length [() | f <- [0..9], b <- [0..9], let n = f + b, even n, n +1 < 10 ]
    25

    - e+c is odd and must carry: 20 cases.
    - d+d+1 must not carry: 5 cases.

    In total: 20*25*20*5.

    When n = 9, an analysis similar to when n = 5 indicates that this is impossible.
 -}

_isReversible :: Int -> Bool
_isReversible x = x `rem` 10 /= 0 && all odd xs
  where
    xs = intToDigitsRev (x + numReverseInBase 10 x)

result :: Int
result = oddCasesCount + evenCasesCount
  where
    computeEvenDigitNums n = 20 * 30 ^! (quot n 2 - 1)
    evenCasesCount = getSum . foldMap computeEvenDigitNums $ [2,4..8]
    -- n = 1, 5, 7 are impossible.
    oddCasesCount =
      100 -- when n = 3
      + 20*25*20*5 -- when n = 7
