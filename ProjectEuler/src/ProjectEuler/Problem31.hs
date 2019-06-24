module ProjectEuler.Problem31
  ( problem
  ) where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 31 Solved result

-- Map (restCoinListLength, subTarget)
type FMemo = M.Map (Int,Int) Int

-- coinSumCount <restCoinList> <subTarget>
coinSumCount :: [Int] -> Int -> State FMemo Int
coinSumCount _ 0 = pure 1
coinSumCount [] _ = pure 0
coinSumCount coins@(x:xs) target = do
    let key = (length coins, target)
    cached <- gets (M.lookup key)
    case cached of
      Just r -> pure r
      Nothing -> do
        let possibleSubTarget =
              map (target-)
              . takeWhile (<= target)
              $ (*x) <$> [0..]
            doFold acc currentTarget = do
              res <- coinSumCount xs currentTarget
              pure $ acc + res
        r <- foldM doFold 0 possibleSubTarget
        modify $ M.insert key r
        pure r

-- version without memoization
_coinSumCount :: [Int] -> Int -> Int
_coinSumCount _ 0 = 1
_coinSumCount [] _ = 0
_coinSumCount (x:xs) target = sum $ _coinSumCount xs <$> possibleSubTarget
  where
    possibleSubTarget = map (target-) $ takeWhile (<=target) $ map (*x) [0..]

result :: Int
result = evalState (coinSumCount coins 200) M.empty
  where
    coins = reverse [1,2,5,10,20,50,100,200]

