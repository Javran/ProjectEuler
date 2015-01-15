import Control.Applicative
import Control.Arrow
import Control.Monad.State
import qualified Data.Map.Strict as M

collatz :: Integer -> Integer
collatz n = if odd n
              then 3 * n + 1
              else n `div` 2

memoCollatz :: Integer
            -> State (M.Map Integer Int, (Integer,Int)) Int
memoCollatz 1 = return 1
memoCollatz n = do
    result <- gets (M.lookup n . fst)
    case result of
        Nothing -> do
            l <- succ <$> memoCollatz (collatz n)
            let update p@(_,curMaxV) =
                    if l > curMaxV
                       then (n,l)
                       else p
            modify (M.insert n l *** update)
            return l
        Just v -> return v

main :: IO ()
main = print $ snd (execState (mapM_ memoCollatz [1..limit]) (M.empty,(1,1)))
  where
    limit = 1000000
