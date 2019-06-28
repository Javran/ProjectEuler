module ProjectEuler.Problem79
  ( problem
  ) where

import Data.List
import Data.Ord
import Petbox

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p79-keylog.txt" 79 Solved compute

hasConsecutiveChar :: String -> Bool
hasConsecutiveChar s = or $ zipWith (==) s (tail s)

isSubstringOf :: Eq a => [a] -> [a] -> Bool
[] `isSubstringOf` _ = True
(x:xs) `isSubstringOf` ys = case ys of
    [] -> False
    (z:zs) -> ((x == z) && xs `isSubstringOf` zs) || (x:xs) `isSubstringOf` zs

solve :: [] Char -> [] Char -> [ [] Char ]
solve passcode key
    | null passcode = [ key ]
    | key `isSubstringOf` passcode = [ passcode ]
    | otherwise = simplify $ do
        let (p1:ps) = passcode
            (k1:ks) = key
        (a,(bs,cs)) <- [ (p1,(ps,key)), (k1,(passcode,ks)) ]
        (a:) <$> solve bs cs

simplify ::[String] -> [String]
simplify = keepMins length . nub . map (map head . group)

keepMins :: Ord b => (a -> b) -> [a] -> [a]
keepMins f xs = filter ((== minV) . f) xs
  where
    minV = f (minimumBy (comparing f) xs)

solveAll :: [String] -> [String] -> [String] -> [String]
solveAll _ passcodes [] = passcodes
solveAll kl solutions (e:es) =
    -- a specialized version of monadic fold,
    -- we make one stopping rule: if there's one passcode
    -- that meets all the requirement, then we don't
    -- need to search any further
    if any isSolution result
      then filter isSolution result
      else solveAll kl result es
    where
      isSolution pc = all (`isSubstringOf` pc) kl
      result = simplify $ do
          pc <- solutions
          solve pc e

compute :: T.Text -> Int
compute raw = read result
  where
    kl = lines $ T.unpack raw
    (result,_) =
      -- we are lucky, since the keys does not have consecutive
      -- digits, we can simplify the algoritm a lot
      if all (not . hasConsecutiveChar . show) kl
        then keepInput length $ head $ simplify $ solveAll kl [""] kl
        else error "Cannot solve"
