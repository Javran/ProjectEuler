module ProjectEuler.Problem151
  ( problem
  ) where

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 151 Unsolved result

{-
  Idea: not sure where to go right now, but there're definitely some pattern
  that we can investigate.

  We'll use notation [a,b,c,d] to denote that:
  - there are `a` pieces of A2 paper in the envelope
  - `b` pieces of A3 paper
  - `c` pieces of A4 paper
  - `d` pieces of A5 paper

  Then, the state after the paper is cut for first batch is always [1,1,1,1]
  and right before the last batch the state is always [0,0,0,1]

  - if we take a piece of A2 paper, it will be cut into A3x1 A4x1 A5x2,
    and one of the resulting A5 paper will be used, resulting in state transition:
    [a,b,c,d] => [a-1,b+1,c+1,d+1]
    Similarly for A3, A4, A5 papers, I'll list them all below:

    - A2 paper taken: [a,b,c,d] => [a-1,b+1,c+1,d+1] (a > 0)
    - A3 paper taken: [a,b,c,d] => [a,b-1,c+1,d+1] (b > 0)
    - A4 paper taken: [a,b,c,d] => [a,b,c-1,d+1] (c > 0)
    - A5 paper taken: [a,b,c,d] => [a,b,c,d-1] (d > 0)

  - also there is a compact way of encoding the state.
    remember that we intentionally start with [1,1,1,1] (i.e. start of second batch),
    by simply counting how many paper of that size is possible to cut from an A1-minus-A5 paper:

    - 0 <= a <= 1 (up to 1 bit)
    - 0 <= b <= 3 (up to 2 bits)
    - 0 <= c <= 7 (up to 3 bits)
    - 0 <= d <= 15 (up to 4 bits)

    So the state can be encoded into a 10 bit number.

 -}

result = ()


