module ProjectEuler.Problem47
  ( problem
  ) where

import Math.NumberTheory.Primes
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 47 Solved result

find4Consecutives :: [Int] -> [Int] -> [Int]
find4Consecutives conList restList
    | [a,b,c,d] <- conList = [d,c,b,a]
    | otherwise =
        let (hd:tl) = restList
        in if length (fst <$> factorise' (fromIntegral hd)) >= 4
          then find4Consecutives (hd:conList) tl
          else find4Consecutives [] tl

result :: Int
result = head $ find4Consecutives [] [1..]

