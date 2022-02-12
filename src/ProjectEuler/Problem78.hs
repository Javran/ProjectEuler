{-# LANGUAGE BlockArguments #-}

module ProjectEuler.Problem78
  ( problem
  )
where

import qualified Data.Array.ST as A
import qualified Data.Array.Unboxed as A
import Petbox
import ProjectEuler.Types

problem :: Problem
problem = pureProblem 78 Solved result

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x : xs) ys = x : interleave ys xs

gpNumbers :: Integral a => [a]
gpNumbers = [(k * (3 * k -1)) `div` 2 | k <- interleave [1 ..] [-1, -2 ..]]

trim :: Int -> Int
trim = (`mod` 1000000)

pFast :: Int -> Int
pFast n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = table A.! n

table :: A.UArray Int Int
table = A.runSTUArray $ do
  -- kind of cheating, the value is first found
  -- and after that we write a faster version
  -- knowing the upbound...
  let limit = 60000
  mary <- A.newArray (0, limit) 0
  A.writeArray mary 0 1
  let writeVal k = do
        let gps = takeWhile (<= k) gpNumbers
        vs <- mapM (\x -> A.readArray mary (k - x)) gps
        let r = trim . sum . zipWith (*) (cycle [1, 1, -1, -1]) $ vs
        A.writeArray mary k r
        pure result
  mapM_ writeVal [1 .. limit]
  pure mary

result :: Int
result =
  fst . firstSuchThat ((== 0) . (`mod` 1000000) . snd)
    . map (keepInput pFast)
    $ [1 ..]
