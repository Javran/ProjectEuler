{-# LANGUAGE TupleSections #-}
import Petbox
{-
import qualified Data.IntMap as IM
import Control.Monad.State
import Control.Monad.Identity
-}
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST as A

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

gpNumbers :: Integral a => [a]
gpNumbers = [ (k*(3*k-1)) `div` 2 | k <- interleave [1..] [-1,-2..] ]

trim :: Int -> Int
trim = (`mod` 1000000)

{-
p :: Int -> State (IM.IntMap Int) Int
p n
    | n < 0  = return 0
    | n == 0 = return 1
    | otherwise = do
        val <- gets $ IM.lookup n
        case val of
            Just v -> return v
            Nothing -> do
                let gps = takeWhile (<= n) gpNumbers
                vs <- mapM (\x -> p (n - x)) gps
                let result = trim . sum . zipWith (*) (cycle [1,1,-1,-1]) $ vs
                modify $ IM.insert n result
                return result

keepInputM :: Functor f => (a -> f b) -> a -> f (a,b)
keepInputM f x = fmap (x,) (f x)
-}

pFast :: Int -> Int
pFast n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = table A.! n
  where
    table = A.runSTUArray $ do
        -- kind of cheating, the value is first found
        -- and after that we write a faster version
        -- knowing the upbound...
        let limit = 60000
        mary <- A.newArray (0,limit) 0
        A.writeArray mary 0 1
        let writeVal k = do
                let gps = takeWhile (<= k) gpNumbers
                vs <- mapM (\x -> A.readArray mary (k - x)) gps
                let result = trim . sum . zipWith (*) (cycle [1,1,-1,-1]) $ vs
                A.writeArray mary k result
                return result
        mapM_ writeVal [1..limit]
        return mary

main :: IO ()
main = print $ firstSuchThat ((== 0) . (`mod` 1000000) . snd)
             $ map (keepInput pFast) [1..]
          -- $ evalState (mapM (keepInputM p) [1..]) IM.empty
