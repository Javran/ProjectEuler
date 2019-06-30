module ProjectEuler.Problem64
  ( problem
  ) where


import Data.List
import Data.Maybe

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 64 Solved result

floorDb :: Double -> Int
floorDb = floor

-- a0 = 4, 1 / (sq 23 - 4)
-- a1 = 1, 7 / (sq 23 - 3)
-- a2 = 3, 2 / (sq 23 - 3)
-- observation:
--   aX = ?, a / (sq n  - b)
-- if the current pair of a,b is:
--   (a,b)
-- then the next pair will be:
--   (k, c*k - b)
-- where
--   k = (n - b^2) / a
--   c = floor ((sq n + b) / k)
-- I don't have any proof, but at least inside the input range n = [1..10000],
-- I find "k" is always an positive integer
next :: Int -> (Int,Int)
     -> Maybe (Int, (Int,Int)) -- not all (sq n)s are quadratic irrationals,
                               -- some of them might to unfold an infinite list (with a loop)
next n (a,b)
      -- all these guards are just empirical observations to eliminate rationals
    | a > 0 && b >= 0 =
        let k = (n - b*b) `div` a
        in if k * a == (n - b*b) && k > 0
           then let c :: Int
                    c = floorDb( ( sqrt (fromIntegral n) + fromIntegral b)
                                 / fromIntegral k)
                in if c*k-b >= 0
                      then Just (c,(k, c*k-b))
                      else Nothing
           else Nothing
    | otherwise = Nothing

-- unfold the list, turns out the pair (a,b) identifies the current state
-- of the iteration, which means once we found any (a,b) appearing more than once
-- in a sequence, we know we have found a loop
gen :: Int -> [ (Int,(Int,Int)) ]
gen n = (b,initPair) : unfoldr next' initPair
  where
    -- "next n" generates just the next digit and a hidden state
    -- "next'" makes the hidden state explicit so that we can detect loops
    next' st = do
        (v,st1) <- next n st
        return ((v,st1),st1)
    initPair = (1,b)
    b = floorDb (sqrt (fromIntegral n))

-- all quadratic irrationals, and only quadratic irrationals,
-- have periodic continued fraction forms.
-- therefore we use "getPeriod" to find out if the sequence contains a loop.
getPeriod :: Ord a => [a] -> Maybe [a]
getPeriod xs = getPeriod' xs []
  where
    getPeriod' [] _ = Nothing
    getPeriod' (y:ys) s
          -- e.g. s = [d,c,b,a], y = b,
          -- then the sequence we've seen so far must be:
          --   a,b,c,d,b, ...
          -- drop everything before "b" will give us that loop,
          -- namely [b,c,d]
        | y `elem` s = Just (dropWhile (/= y). reverse $ s)
        | otherwise  = getPeriod' ys (y:s)

result :: Int
result =
  length
  -- collect those whose periods are odds
  . filter (odd . length)
  -- collect those that have periods
  . mapMaybe (getPeriod . gen)
  $ [1..10000]

