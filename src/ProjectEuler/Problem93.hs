module ProjectEuler.Problem93
  ( problem
  ) where

import Data.Ratio
import qualified Data.IntSet as IS
import Petbox
import Data.List
import Data.Ord

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 93 Solved result

searchSpace :: [ [Ratio Int] ]
searchSpace = do
    d <- [1..9]
    c <- [1..d-1]
    b <- [1..c-1]
    a <- [1..b]
    return $ map (%1) [a,b,c,d]

ambArith :: [Ratio Int] -> [Ratio Int]
ambArith [] = []
ambArith [x] = [x]
ambArith xs = do
    let pickOne ys = map (take l) . take l . iterate tail $ cycle ys
          where l = length ys
    (n1:ns1) <- pickOne xs
    (n2:ns2) <- pickOne ns1
    op <- [(+),(-),(*)] ++ [(/) | n2 /= 0]
    ambArith $ (n1 `op` n2):ns2

onlyInts :: [Ratio Int] -> IS.IntSet
onlyInts = IS.fromList
         . map numerator
         . filter ((== 1) . denominator)

maxObtainable :: IS.IntSet -> Int
maxObtainable xs = firstSuchThat (`IS.notMember` xs) [1..] - 1

result :: Int
result =
    foldl (\a b -> a*10 + round b) 0
    . fst
    . maximumBy (comparing snd)
    . map (keepInput solve)
    $ searchSpace
  where
    solve = maxObtainable . onlyInts . ambArith


