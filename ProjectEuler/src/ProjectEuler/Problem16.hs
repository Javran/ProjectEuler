module ProjectEuler.Problem16
  ( problem
  ) where


import Data.Char
import Prelude hiding ((^))
import qualified Prelude ((^))
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 16 Solved result

infixr 8 ^

(^) :: Num a => a -> Int -> a
x ^ n = x Prelude.^ n

result :: Int
result = sum $ map digitToInt $ show (2 ^ 1000 :: Integer)

