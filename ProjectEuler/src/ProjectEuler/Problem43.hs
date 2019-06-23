module ProjectEuler.Problem43
  ( problem
  ) where

import Control.Monad
import qualified Data.List.Match as LMatch

import ProjectEuler.Types

{-
  we could try every permutation, which is straightforward to do,
  but the search space is huge.

  so instead, we can do a search begin with a smaller branching factor -
  pick d10, d9, d8 first with 3 distinct numbers, verify they
  are divisible by 17, then choose d7, check with 13, then choose d6,
  check with 11, etc.

  this approach trims the branch very early: as soon as the current
  divisible property cannot be satisfied, there is no need of going any further,
  therefore search space is reduced to a small set of values very quickly.
 -}

problem :: Problem
problem = pureProblem 43 Solved result

pickOne :: [a] -> [(a, [a])]
pickOne xs = LMatch.take xs $ (\(a,b:c) -> (b,a<>c)) . (`splitAt` xs) <$> [0..]

toNum :: [Int] -> Int
toNum = foldl (\acc i -> acc*10 + i) 0

solution :: [[Int]]
solution = do
    (d10, xs) <- pickOne [0..9]
    (d9, ys) <- pickOne xs
    solve' [d9,d10] ys initPrimeList
  where
    initPrimeList = [17,13,11,7,5,3,2]
    solve' chosenList remained primeList
      | null primeList = pure (remained <> chosenList)
      | otherwise = do
          -- choose x,
          -- and look into current chosen list of digits to get y and z
          (x,remained') <- pickOne remained
          let (y:z:_) = chosenList
              chosenList' = x : chosenList
              (p:primeList') = primeList
              n = x * 100 + y * 10 + z
          guard $ n `rem` p == 0
          solve' chosenList' remained' primeList'

result :: Int
result = sum $ toNum <$> solution

