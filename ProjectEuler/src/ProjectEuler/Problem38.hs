module ProjectEuler.Problem38
  ( problem
  ) where

import Data.List
import Data.Ord
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 38 Solved result

-- enumerate through permutation should be more efficient
--   first thing is how we break the list (of 9 elements)
-- denote [xx,xx,xxx,xxx,xxx] to be [2,2,3,3,3] i.e. the length of each element
-- we want:
-- * sum [?,?,..,?] == 9
-- * [?,?,..,?] can only follow the pattern [a,a,..,a,b,b..,b] where b = a + 1
--   (because x*9 is at most one more digit than x)
-- * a*c + b*d == 9, c >= 1, d >= 0, b = a + 1, c + d > 1
--   => a*(c+d) == 9 - d
possibleSchemes :: [[Int]]
possibleSchemes =
  [ replicate c a ++ replicate d (a+1)
  | a <- [1..9]
  , d <- [0..9]
  , c <- [1..9]
  , a * (c+d) == 9 - d
  , c + d > 1
  ]

splitBy :: [Int] -> [Int] -> [[Int]]
splitBy scheme xs = reverse $ fst $ foldl doSplit ([],xs) scheme
  where
    doSplit (done,todo) i = (hdPart:done, tlPart)
      where
        (hdPart, tlPart) = splitAt i todo

valid :: [Int] -> Bool
valid nums = all (uncurry (==)) $ zip nums [hd,hd*2..]
  where
    hd = head nums

result :: Int
result = read (concatMap show xs)
  where
    solutions =
      [ nums
      | perm <- permutations [1..9]
      , oneScheme <- (`splitBy` perm) <$> possibleSchemes
      , let nums = digitsToInt <$> oneScheme
      , valid nums
      ]
    xs = maximumBy (comparing head) solutions :: [Int]
