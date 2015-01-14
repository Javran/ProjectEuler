import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Monad.State

-- Map (restCoinListLength, subTarget)
type FunMemo = M.Map (Int,Int) Int

-- coinSumCount <restCoinList> <subTarget>
coinSumCount :: [Int] -> Int -> State FunMemo Int
coinSumCount _ 0 = return 1
coinSumCount [] _ = return 0
coinSumCount coins@(x:xs) target = do
    let key = (length coins, target)
    cached <- gets (M.lookup key)
    let calcF = do
            let possibleSubTarget = map (target-) $ takeWhile (<=target) $ map (*x) [0..]
            let doFold acc currentTarget = do
                res <- coinSumCount xs currentTarget
                return $ acc + res
            result <- foldM doFold 0 possibleSubTarget
            modify $ M.insert key result
            return result

    maybe calcF return cached

coinSumCount1 :: [Int] -> Int -> Int
coinSumCount1 _ 0 = 1
coinSumCount1 [] _ = 0
coinSumCount1 (x:xs) target = sum $ map (coinSumCount1 xs) possibleSubTarget
    where
        possibleSubTarget = map (target-) $ takeWhile (<=target) $ map (*x) [0..]

main = do
    let coins = reverse [1,2,5,10,20,50,100,200]
    print $ evalState (coinSumCount coins 200) M.empty
    --print $ coinSumCount1 coins 200
