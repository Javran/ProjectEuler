module ProjectEuler.Problem86
  ( problem
  ) where

import Petbox
import Control.Monad

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 86 Solved result

-- generate pythagorean triples
-- we only need the first two sides
-- maxL controls the maximum length of a single cube side
genTwoSides :: Int -> [(Int,Int)]
genTwoSides maxL = do
    -- maxL*3 is just an approximated choice
    -- no need to be accurate here
    m <- [1..maxL*3]
    n <- [1..m]
    guard $ odd (m - n) && gcd m n == 1
    let a = sq m - sq n
        b = 2*m*n
    -- here sum of the first 2 sides of the pythagorean triple
    -- should be the same as the sum of three sides of the cube
    -- thus cannot exceed maxL*3
    k <- takeWhile (\k' -> k'*(a+b) <= maxL*3 ) [1..]
    return (a*k,b*k)

-- given first two sides (non-slopes) of the triple,
-- count number of possible partitions (a,b,c)
-- in which a >= b && b >= c && c > 0 && a <= m && a+b+c==x+y
possiblePartitiions :: Int -> (Int,Int) -> Int
possiblePartitiions m (x,y) = count x y + count y x
  where
    count a plusBC =
        if a > m
          then 0
          else let l = halve (plusBC+1)
                   r = plusBC-1
               in max 0 ((min a r - l) + 1)

result :: Int
result = binSearch 1500 2000
  where
    -- 1500 and 2000 are magic numbers, we need to find an approximated range to search
    limitedPyth = genTwoSides 2000
    tryNum m = sum $ map (possiblePartitiions m) limitedPyth
    -- tryNum is monotonic, do a binary search
    binSearch l r
        | l == mid = l -- TODO: r = l or r = l+1
          -- now that l /= mid, which means r > l+1
          -- given r /= l, we have: mid < r, so [l..mid] is guaranteed
          -- to be strictly smaller than [l..r]
        | tryNum mid > 1000000 = binSearch l mid
          -- mid < r => mid+1 <= r
        | otherwise = binSearch (mid+1) r
      where
        mid = halve (l+r)

