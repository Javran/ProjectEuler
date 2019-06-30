module ProjectEuler.Problem104
  ( problem
  ) where

import qualified Data.Set as S

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 104 Solved result

fibs :: [Integer]
fibs = 0:1:zipWith (+) (tail fibs) fibs

-- internal only. as we are only interest in some particular parts
-- of the "show" result of some Integer.
isPandigital' :: String -> Bool
isPandigital' xs = S.size charNum == 9
  where
    charNum = S.delete '0' . S.fromList $ xs

fstPandigital :: Integer -> Bool
fstPandigital n = isPandigital' (take 9 (show n))

lstPandigital :: Integer -> Bool
lstPandigital n = isPandigital' (show $ n `mod` 1000000000)

result :: Integer
result =
  fst $ head $ filter (\(_, x) -> lstPandigital x && fstPandigital x)
      $ zip [0..] fibs
