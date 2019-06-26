module ProjectEuler.Problem51
  ( problem
  ) where

import Control.Monad
import Math.NumberTheory.Primes

import ProjectEuler.Types
import ProjectEuler.Problem35 (intToDigits, digitsToInt)

problem :: Problem
problem = pureProblem 51 Solved result

type Digits = [Int]

-- search space, we are looking for
-- prime numbers that contains '1'.
searchSpace :: [Digits]
searchSpace = do
  n <- primes
  let ds = intToDigits n
  guard (1 `elem` ds)
  pure ds

-- from a number that contains 1 to all posible masks
--   here is `1` plays the role of indicating that
--   this position is suitable of masking
--   a mask is a list of positions
-- e.g.: 3121
-- => [[1,3],[1],[3]]
oneNumToMask :: Digits -> [[Int]]
oneNumToMask xs =
    -- first element is empty therefore ignore it.
    tail $ powerset onePos
  where
    -- all position of ones
    --   all possible masks is just the non-empty powerset of this
    onePos = map fst $ filter ((== 1). snd) $ zip [0..] xs

-- compute the powerset of a list
powerset :: [a] -> [[a]]
powerset = filterM (const [False, True])

-- apply the mask, get the family of 10 numbers
applyMask :: Digits -> [Int] -> [Int]
applyMask digits mask = doAll <$> transRange
  where
    doAll :: Int -> Int
    doAll =
      -- 3. convert them back into integers
      digitsToInt
      -- 2. apply transformers so we get the family in their string form
      . ($ indexedDigits)
      -- 1. make transformers from numbers
      . transformTo

    indexedDigits = zip [0..] digits
    transRange = if 0 `elem` mask then [1..9] else [0..9]

    -- transformTo takes a number `n`, and then an `indexedNStr`
    --   changes masked digits to `n` according to the setting of `mask`
    --   e.g.:
    --   zip [0..] "12311"
    --   transformTo 9 (zip [0..] "12311"),
    --                             ^  ^
    --                     mask = [0__3_]
    --   => "92391"
    transformTo :: Int -> [(Int,Int)] -> [Int]
    transformTo n = map (\(i,v) -> if i `elem` mask then n else v)

result :: Int
result = head $ head answers
  where
    answers = do
      -- num that contains 1
      nDigits <- searchSpace
      -- pick up one of the possible mask
      mask <- oneNumToMask nDigits
      -- apply the mask to obtain the prime number family
      -- the pattern match takes the advantage of List being a MonadFail
      -- to only allow list of exactly 8 elements.
      family@[_1,_2,_3,_4,_5,_6,_7,_8] <-
        [filter (isPrime . fromIntegral) $ applyMask nDigits mask]
      pure family
