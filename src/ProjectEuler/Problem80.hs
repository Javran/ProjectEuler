module ProjectEuler.Problem80
  ( problem
  ) where

import Math.NumberTheory.Powers.Squares
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 80 Solved result

-- see: http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation
next :: Integer -> [Integer] -> (Integer,[Integer])
next p (c:c':cs) = (p*10+x,(c-y)*100+c':cs)
  where
    x = lastSuchThat (\x' -> x'*(20*p+x') <= c) [0..9]
    y = x * (20*p+x)
next _ _ = error "unreachable"

genDigits :: Int -> Int -> Integer
genDigits limit n = fst $ iterate (uncurry next) (0,split100 n) !! limit

isIrrationalSquareRoot :: Int -> Bool
isIrrationalSquareRoot = not . isSquare'

split100 :: (Show a, Integral a) => a -> [Integer]
split100 n = map (read . take 2) $ iterate (drop 2) (sn' ++ repeat '0')
  where
    sn = show n
    sn' = if odd (length sn) then '0':sn else sn

result :: Int
result =
  sum
  . map (sum . take 100 . intToDigits . genDigits 100)
  . filter isIrrationalSquareRoot
  $ [1..100]

