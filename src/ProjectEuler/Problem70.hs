{-# LANGUAGE RankNTypes #-}
module ProjectEuler.Problem70
  ( problem
  ) where


import Data.Ratio
import Data.Function
import Data.List
import Petbox

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 70 Solved result


{-
  we know previously that:

    target = n / phi(n) = 1 / product { 1-1/p_i } where p_i are unique primes of n

  to minimize the target, we need to maximize the production

  since p_a > p_b > 1 ==> 1 > 1 - 1/p_a > 1 - 1/p_b > 0, we need to:

  - minimize the number of terms
  - maximize p

  at least two terms are needed, or otherwise n is a prime and phi(n) = n-1
  can never be a permutation of n

  we now focus on n that has only two unique prime factors,

  n should have form: n = p^a * q^b

  since we want to maxmize p and q, it's a waste to use p^a (a > 1) when a bigger
  prime number can be used in place of it, so let's just say a = b = 1
  (the production should be less than 10,000,000 of course)

  so we can solve this problem taking following steps:

  - generate prime pairs whose productions are less than 10,000,000
  - filter through the list, leaving only those that
    meet the permutation requirement
  - pick up one that has the minimum n / phi(n) = pq / (p-1)*(q-1)

  P.S. thanks for the hints from:
  http://www.mathblog.dk/project-euler-70-investigate-values-of-n-for-which-%cf%86n-is-a-permutation-of-n/
-}

isPermutationOf :: Eq a => [a] -> [a] -> Bool
[] `isPermutationOf` [] = True
(x:xs) `isPermutationOf` ys = maybe False (isPermutationOf xs) (deleteOne x ys)
  where
    deleteOne _ [] = Nothing
    deleteOne a (b:bs) =
        if a == b
          then Just bs
          else deleteOne a bs >>= Just . (b:)
_ `isPermutationOf` _ = False

isNumPermOf :: Int -> Int -> Bool
isNumPermOf a b = isPermutationOf sa sb
  where
    sb = show b
    l = length sb
    sa = reverse . take l $ (reverse (show a) ++ repeat '0')

solution :: ((Int, Int), Ratio Int)
solution = minimumBy (compare `on` snd)
          . map (keepInput (\(p,q) -> (p*q) % ((p-1) * (q-1))))
          . filter isValid
          . concatMap (\(p:ps) -> takeWhile ((<= limit) . uncurry (*))
                                  $ zip (repeat p) ps)
          . takeWhile (not . null)
          . iterate tail
          $ primes'
  where
    limit = 10000000
    isValid :: (Int,Int) -> Bool
    isValid (p,q) = ((p-1)*(q-1)) `isNumPermOf` (p*q)
    primes' = takeWhile (<= limit) primes

result :: Int
result = a*b
  where
    ((a,b), _) = solution
