module ProjectEuler.Problem125
  ( problem
  ) where

import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 125 Unsolved result

{-
  Idea: given n, we know that

  1^2 + 2^2 + ... n^2 = (n * (n+1) * (2n+1)) / 6

  using this, we can easily compute sum of squares
  for one number m to another one n.

  To find the upperbound:

  > let f n = (n * (n+1) * (2*n+1)) `quot` 6
  > f 1
  1
  > f 2
  5
  > f 6
  91
  > head $ dropWhile ((< 100000000) . f) [1..]
  669

  Doesn't sound like a lot to just brute force.

  Update: not so fast though. The upperbound does not work this way:
  we want to actually upperbound on the difference, meaning
  that the actual sum might be way exceeding 10^8,
  perhaps we can try fixing the initial number to i,
  try i^2 + (i+1)^2 ... until it exceeds the maximum,
  then try (i+1)^2 as the initial one.

 -}

-- ported from p55
isPalindrome :: Int -> Bool
isPalindrome x = x == numReverse x

numReverse :: Int -> Int
numReverse = numReverseInBase 10

maxV :: Int
maxV = 100000000

partialAns :: Int -> [Int]
partialAns n = filter isPalindrome sqSum
  where
    sqSum =
      takeWhile (< maxV)
      . drop 1 -- dropping the first one to limit it to more than one number
      . scanl1 (+)
      $ (\i -> i*i) <$> [n..]

result :: Int -- TODO: could it be possible that the generated list is containing non-uniques?
result = sum $ do
  i <- [1 .. 10000]
  partialAns i
