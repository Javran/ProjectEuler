import Data.Numbers.Primes
import Data.Foldable

import qualified Data.IntSet as IS
import Prelude hiding (product,sum)

-- see: http://mathschallenge.net/library/number/sum_of_divisors
divisorSum :: Integral a => a -> a
divisorSum 1 = 1
divisorSum n = product . map tranform . factorization $ n
  where
    tranform (p,t) = (p ^ (t+1) - 1) `div` (p - 1)

factorization :: Integral a => a -> [(a, Int)]
factorization n
    | n <= 1    = error "must be greater than 1"
    | otherwise = factorization' n primes
  where
    factorization' _ [] = undefined -- impossible
    factorization' 1 _ = []
    factorization' m (p:ps)
        | m `mod` p == 0 = let (times,pn) = last
                                          . takeWhile ((== 0) . (m `mod`) . snd)
                                          . zip [1..]
                                          $ iterate (*p) p
                           in (p,times) : factorization' (m `div` pn) ps
        | otherwise = factorization' m ps

isAbundant :: Int -> Bool
isAbundant n = n < divisorSum n - n

main :: IO ()
main = do
    let possibleAbuns = takeWhile (<= maxAbun) $ filter isAbundant [1..]
        maxAbun = 28123
        -- TODO:
        -- seems like the list comprehension version is slower
        -- not sure why
        reachables = IS.fromList $ do
          x <- possibleAbuns
          y <- takeWhile (\u -> u + x <= maxAbun) possibleAbuns
          return (x+y)
    print $ sum $ IS.toList $ IS.fromList [1..maxAbun] IS.\\ reachables
