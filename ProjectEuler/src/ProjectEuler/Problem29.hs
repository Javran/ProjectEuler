module ProjectEuler.Problem29
  ( problem
  ) where

import qualified Data.Set as S
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 29 Solved result

result :: Int
result = S.size $ S.fromList [x^y | x<-[2..100 :: Integer], y<-[2..100 :: Int]]

