module ProjectEuler.Problem32
  ( problem
  ) where

import Control.Monad
import Data.List

import ProjectEuler.Types
import ProjectEuler.Problem24 (getPermutation,factorial)

problem :: Problem
problem = pureProblem 32 Solved result

threeParts :: [(Int, Int, Int)]
threeParts = do
  a <- [1..9]
  b <- [a..9]
  let c = 9-a-b
  guard $ a+b-1 <= c && c <= a+b
  pure (a,b,c)

threePartition :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
threePartition xs (a,b,c) = (digitToNum aPart, digitToNum bPart, digitToNum cPart)
    where
      (aPart, xs1) = splitAt a xs
      (bPart, xs2) = splitAt b xs1
      (cPart, _) = splitAt c xs2

digitToNum :: [Int] -> Int
digitToNum = foldl (\acc i -> acc*10+i) 0

result :: Int
result =sum . nub . map (\(_,_,z) -> z) $ answers
  where
    allPerms = map (`getPermutation` [1..9]) [0..factorial 9 -1]
    searchSpace = concatMap (\perm -> map (threePartition perm) threeParts) allPerms
    valid (x,y,z) = x*y == z
    answers = filter valid searchSpace

