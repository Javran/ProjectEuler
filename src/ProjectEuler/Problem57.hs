module ProjectEuler.Problem57
  ( problem
  ) where

import Data.Ratio
import Data.Function

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 57 Solved result

gShared :: [Ratio Integer]
gShared = iterate (\x -> 2 + 1 / x) 2

g :: Int -> Ratio Integer
g n = gShared !! n
-- g 0 = 2
-- g n = 2 + 1 / g (n-1)

f :: Int -> Ratio Integer
f n = 1 + 1 / g n

result :: Int
result = length . filter isValid $ expansions
  where
    expansions = take 1000 $ map f [0..]
    -- number of digits in numerator exceeds denominator
    isValid = ((>) `on` (length . show)) <$> numerator <*> denominator

