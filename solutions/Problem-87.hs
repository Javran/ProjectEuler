import Petbox
import qualified Data.IntSet as IS

limit :: Int
limit = 50000000

searchSpace :: [Int]
searchSpace = do
    pFt <- takeWhile  (<= limit)                $ map (^! 4) primes
    pCb <- takeWhile ((<= limit).(+pFt))        $ map (^! 3) primes
    pSq <- takeWhile ((<= limit).(+(pFt+pCb)))  $ map (^! 2) primes
    return (pSq + pCb + pFt)

main :: IO ()
main = print $ IS.size $ IS.fromList searchSpace
