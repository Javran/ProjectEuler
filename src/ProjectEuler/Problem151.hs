{-# LANGUAGE BangPatterns #-}
module ProjectEuler.Problem151
  ( problem
  ) where

import Control.Monad
import Control.Monad.State
import Data.Function
import System.Random.TF
import System.Random.TF.Instances
import Data.List

import ProjectEuler.Types

problem :: Problem
problem = Problem 151 Unsolved run

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

    So the state can be encoded into a 10 bit number.

  Well, actually, we can try some simulation first and see if leads us
  to an correct answer.

 -}

data Envelope = Envelope Int Int Int Int deriving Show

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

experiments :: TFGen -> [] (Int, Int)
experiments g0 = -- TODO: accumulate.
  zip (unfoldr (Just . runState experiment) g0) [1..]

run = do
  g <- liftIO newTFGen
  logT (take 20 $ experiments g)
