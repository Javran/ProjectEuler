{-# LANGUAGE BangPatterns #-}
module ProjectEuler.Problem151
  ( problem
  ) where

import Petbox
import Control.Monad
import Control.Monad.State
import Data.Function
import System.Random.TF
import System.Random.TF.Instances
import Data.List
import Data.Bits
import Data.Ratio
import Text.Printf
import TextShow

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblem 151 Solved result

{-
  Idea: not sure where to go right now, but there're definitely some pattern
  that we can investigate.

  We'll use notation [a,b,c,d] to denote that:
  - there are `a` pieces of A2 paper in the envelope
  - `b` pieces of A3 paper
  - `c` pieces of A4 paper
  - `d` pieces of A5 paper

  Then, the state after the paper is cut for first batch is always [1,1,1,1]
  and right before the last batch the state is always [0,0,0,1]

  - if we take a piece of A2 paper, it will be cut into A3x1 A4x1 A5x2,
    and one of the resulting A5 paper will be used, resulting in state transition:
    [a,b,c,d] => [a-1,b+1,c+1,d+1]
    Similarly for A3, A4, A5 papers, I'll list them all below:

    - A2 paper taken: [a,b,c,d] => [a-1,b+1,c+1,d+1] (a > 0)
    - A3 paper taken: [a,b,c,d] => [a,b-1,c+1,d+1] (b > 0)
    - A4 paper taken: [a,b,c,d] => [a,b,c-1,d+1] (c > 0)
    - A5 paper taken: [a,b,c,d] => [a,b,c,d-1] (d > 0)

  - also there is a compact way of encoding the state.
    remember that we intentionally start with [1,1,1,1] (i.e. start of second batch),
    by simply counting how many paper of that size is possible to cut from an A1-minus-A5 paper:

    - 0 <= a <= 1 (up to 1 bit)
    - 0 <= b <= 3 (up to 2 bits)
    - 0 <= c <= 7 (up to 3 bits)
    - 0 <= d <= 15 (up to 4 bits)

    So the state can be encoded into a 10-bit number.

  Well, actually, we can try some simulation first and see if leads us
  to an correct answer.
  Update: we are looking for some number around:

  (29572856,20194501)
  0.46440142294181963

  (3868498,2642501)
  0.46395327759573224

  (23265801,15888501)
  0.464316929583225

  with threshold = 1/100000000, and `sample 500` applied.

 -}


data Envelope = Envelope !Int !Int !Int !Int deriving (Show, Eq)

ePack :: Envelope -> Int
ePack (Envelope a b c d) = d .|. shiftL c 4 .|. shiftL b 7 .|. shiftL a 10

f :: Int -> Rational
f ev
  | tot == 0 = 0
  | otherwise =
      let cur = if tot == 1 then 1 else 0
          caseA =
            if a > 0 then f (ePack $ Envelope (a-1) (b+1) (c+1) (d+1)) else 0
          caseB =
            if b > 0 then f (ePack $ Envelope a (b-1) (c+1) (d+1)) else 0
          caseC =
            if c > 0 then f (ePack $ Envelope a b (c-1) (d+1)) else 0
          caseD =
            if d > 0 then f (ePack $ Envelope a b c (d-1)) else 0
      in cur + caseA * (fInt a % tot) + caseB * (fInt b % tot) + caseC * (fInt c % tot) + caseD * (fInt d % tot)
  where
    Envelope a b c d = eUnpack ev
    tot = fInt $ a + b + c + d

eUnpack :: Int -> Envelope
eUnpack v = Envelope a b c d
  where
    d = v .&. 15
    c = shiftR v 4 .&. 7
    b = shiftR v 7 .&. 3
    a = shiftR v 10 .&. 1

nexts :: Envelope -> [] Envelope
nexts (Envelope a b c d) =
    pickA2 <> pickA3 <> pickA4 <> pickA5
  where
    pickA2 = replicate a $ Envelope (a-1) (b+1) (c+1) (d+1)
    pickA3 = replicate b $ Envelope a (b-1) (c+1) (d+1)
    pickA4 = replicate c $ Envelope a b (c-1) (d+1)
    pickA5 = replicate d $ Envelope a b c (d-1)

experiment :: State TFGen Int
experiment =
    fix
      (\loop e@(Envelope a b c d) !count -> do
          let l = a + b + c + d
          ind <- state (randomR (0, l-1))
          let e'@(Envelope a' b' c' d') = nexts e !! ind
              l' = a' + b' + c' + d'
              count' = if l' == 1 then count + 1 else count
          if l' == 0
            then pure count'
            else loop e' count'
         )
      e0 0
  where
    e0 = Envelope 1 1 1 1

experiments :: TFGen -> [] Int
experiments = unfoldr (Just . runState experiment)

{-
  pair <numerator, denominator>.
 -}
averages :: TFGen -> [] (Int, Int)
averages g = zip accumulated [1..]
  where
    accumulated = scanl1 (+) $ experiments g

{-
  here the threshold is 1 / thresRcpl (i.e. the reciprocal)
  also it is expected that xs is infinite.
 -}
findFixpoint :: Int -> [] (Int, Int) -> (Int, Int)
findFixpoint thresRcpl xs =
    snd $ firstSuchThat withinThreshold zs
  where
    withinThreshold ((a,b),(c,d)) =
      (fInt thresRcpl :: Integer) * abs (fInt a * fInt d - fInt b * fInt c) < fInt b * fInt d
    zs = zip xs (tail xs)

-- for only taking samples by some distances in between.
-- this is an attempt of eliminating the instability cause by "outliers"
sample :: Int -> [a] -> [a]
sample n (x:xs) = x : sample n (drop (n-1) xs)

run = do
  g <- liftIO newTFGen
  let z@(n,d) = findFixpoint 100000000 (sample 500 $ averages g)
  logT z
  -- minus one one the result as the last batch doesn't count.
  logT (fInt (n - d) / fInt d :: Double)

newtype Rounded = Rounded Double

instance TextShow Rounded where
  showb (Rounded v) = fromString (printf "%.6f" v)

-- TODO: cleanup pending.
result = Rounded (fromRational (f (ePack $ Envelope 1 1 1 1) - 1) :: Double)
