module ProjectEuler.Problem90
  ( problem
  ) where

import qualified Data.IntSet as  IS
import qualified Data.Set as S
import Control.Monad
import Data.List

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 90 Solved result

type Cube = (IS.IntSet,[Int])

canDisplay :: (Cube,Cube) -> (Int,Int) -> Bool
(~(c1,_),~(c2,_)) `canDisplay` (hD,lD) = (c1 `canDisplayDigit` hD && c2 `canDisplayDigit` lD)
                                      || (c2 `canDisplayDigit` hD && c1 `canDisplayDigit` lD)
  where
    canDisplayDigit cx d
        | d == 6 || d == 9 = 6 `IS.member` cx || 9 `IS.member` cx
        | otherwise = d `IS.member` cx

squareNums :: [(Int,Int)]
squareNums = [(0,1),(0,4),(0,9)
             ,(1,6),(2,5),(3,6)
             ,(4,9),(6,4),(8,1)]

cubes :: [Cube]
cubes = do
    a <- [0..9]
    b <- [0..a-1]
    c <- [0..b-1]
    d <- [0..c-1]
    e <- [0..d-1]
    f <- [0..e-1]
    let cb = [f,e,d,c,b,a]
    return (IS.fromAscList cb,cb)

result :: Int
result = S.size $ S.fromList $ do
    (c1:restCubes) <- tails cubes
    c2 <- restCubes
    guard $ all ((c1,c2) `canDisplay`) squareNums
    pure (c1,c2)

