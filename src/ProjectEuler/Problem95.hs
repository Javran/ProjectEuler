module ProjectEuler.Problem95
  ( problem
  ) where

import Math.NumberTheory.Primes
import Math.NumberTheory.ArithmeticFunctions
import Control.Monad.State
import Data.Maybe

import qualified Data.List.Ordered as LOrdered
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 95 Unsolved result

{-

  Some ideas:
  - https://en.wikipedia.org/wiki/Sociable_number
  - https://en.wikipedia.org/wiki/Aliquot_sequence

  1,000,000 numbers could be searchable - but it's still too slow.

  new idea: if aliquot sequence runs into prime, we should never include
  that as a candidate, that gives us only 13862 to search, but
  getting this list could be slow.

  explored: if we do only one step (with Maybe to eliminate candidates)
  at a time, it'll still take a while to get to the 7th interation.

  update: nub through IntSet eliminates more, but is still slow.

  TODO: memoization?

 -}

maxN :: Int
maxN = 1000000

-- initSearchSpace :: IS.IntSet
initSearchSpace = [1..maxN] `LOrdered.minus` primes

aliquotSet :: Int -> Maybe Int
aliquotSet 0 = Nothing
aliquotSet 1 = Nothing
aliquotSet n = do
  s <- aliquotSet' IS.empty n
  pure . sum $ IS.toList s

sumOfProperDivisors :: Int -> State (IM.IntMap Int) Int
sumOfProperDivisors n
  | n <= 1 = pure 0
  | isPrime (fromIntegral n) = pure 1
  | otherwise = do
      z <- gets (IM.lookup n)
      case z of
        Nothing -> do
          let v = (sum . IS.toList $ divisorsSmall n) - n
          modify (IM.insert n v)
          pure v
        Just v -> pure v

next :: Int -> Maybe (Int, Int)
next 0 = Nothing
next 1 = Nothing
next n = if vs == [1] then Nothing else Just (n, x)
  where
    vs = init . IS.toList $ divisorsSmall n
    x = sum vs

aliquotSet' :: IS.IntSet -> Int -> Maybe IS.IntSet
aliquotSet' s 1 = Nothing
aliquotSet' s n
  | isPrime (fromIntegral n) = Nothing
  | IS.member n s = Just s
  | n > maxN = Nothing
  | otherwise = aliquotSet' (IS.insert n s) next
  where
    next = (sum . IS.toList $ divisorsSmall n) - n

{-
search :: IS.IntSet -> [IS.IntSet] -> [IS.IntSet]
search searchSpace groups = case IS.maxView searchSpace of
  Nothing -> groups
  Just (x, _) ->
    let aliquotX = aliquotSet x
        searchSpace' = searchSpace `IS.difference` aliquotX
    in search searchSpace' (mergeIntoGroups groups aliquotX)
 -}

mergeIntoGroups :: [IS.IntSet] -> IS.IntSet -> [IS.IntSet]
mergeIntoGroups [] s = [s]
mergeIntoGroups (x:xs) s =
  if IS.null (IS.intersection x s)
    then x : mergeIntoGroups xs s
    else IS.union x s : xs

{-
result2 = length $
  iterate
    (IS.toList . IS.filter (not . isPrime . fromIntegral) . IS.fromList . mapMaybe next)
    initSearchSpace
  !! 2
-}
-- looks like memoization is faster.
{-result = (`evalState` IM.empty) $ do
  ns <- mapM sumOfProperDivisors initSearchSpace
  pure (sum ns)
-}

-- just want to see how many that appears both as input and output
result = IS.size (IS.intersection srcs dsts)
  where
    srcs = IS.fromDistinctAscList $ map fst pairs
    dsts = IS.fromList $ map snd pairs
    pairs = mapMaybe next initSearchSpace
