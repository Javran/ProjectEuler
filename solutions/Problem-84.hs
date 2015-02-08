{-# LANGUAGE TemplateHaskell, BangPatterns #-}
import Control.Lens
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Random
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Array as A
import Data.Function
import Text.Printf
import Petbox

data CCCard
  = CCTo Int
  | CCNone

data CMCard
  = CMTo Int
  | CMNext Int -- 0 for R, 1 for U
  | CMBack Int
  | CMNone

cellNames :: [String]
cellNames = words  "GO   A1 CC  A2  T1 R1 B1  CH1 B2 B3 \
                   \JAIL C1 U1  C2  C3 R2 D1  CC2 D2 D3 \
                   \FP   E1 CH2 E2  E3 R3 F1  F2  U2 F3 \
                   \G2J  G1 G2  CC3 G3 R4 CH3 H1  T2 H2"

-- just for sharing the data
posToNameD :: A.Array Int String
posToNameD = A.array (0,39) $ zip [0..] cellNames

nameToPosD :: M.Map String Int
nameToPosD = M.fromList $ zip cellNames [0..]

posToName :: Int -> String
posToName = (posToNameD A.!) . (`mod` 40)

nameToPos :: String -> Int
nameToPos n = fromJust (M.lookup n nameToPosD)

cmNext :: Int -> Int -> Int
cmNext curPos eitherRU = nameToPos $ firstSuchThat isTarget squares
  where
    isTarget = (== (if eitherRU == 0 then 'R' else 'U')) . head
    squares = drop (succ curPos) $ cycle cellNames

data GameState = GameState
  { _currentPos :: Int
  , _doubleCount :: Int
  , _ccPile :: [CCCard]
  , _cmPile :: [CMCard]
  , _stat :: IM.IntMap Int
  }
makeLenses ''GameState

initState :: GameState
initState = GameState 0 0 ccPile' cmPile' IM.empty
  where
    ccPile' = cycle
            . take 16
            $ [ CCTo (nameToPos "GO")
              , CCTo (nameToPos "JAIL")
              ] ++ repeat CCNone
    cmPile' = cycle
            . take 16
            $ [ CMTo (nameToPos "GO")
              , CMTo (nameToPos "JAIL")
              , CMTo (nameToPos "C1")
              , CMTo (nameToPos "E3")
              , CMTo (nameToPos "H2")
              , CMTo (nameToPos "R1")
              , CMNext 0
              , CMNext 0
              , CMNext 1
              , CMBack 3 ] ++ repeat CMNone

oneStep :: StateT GameState IO ()
oneStep = do
    -- roll dices
    let rollDice = getRandomR (1 :: Int, 4)
    [d1,d2] <- replicateM 2 rollDice
    modify (if d1 == d2 then doubleCount %~ succ
                        else doubleCount .~ 0)
    dc <- gets (view doubleCount)
    if dc == 3
       then modify ( (doubleCount .~ 0)
                   . (currentPos .~ pJail) )
       else do
         -- advance
         modify (currentPos %~ (\cp -> (cp+d1+d2) `mod` 40))
         let toFixpoint = do
                 curPos <- gets (view currentPos)
                 let curCell = posToName curPos
                 case curCell of
                     "G2J" -> modify (currentPos .~ pJail)
                     'C':'C':_ -> do
                         card <- nextCC
                         case card of
                             CCTo v -> modify (currentPos .~ v)
                             CCNone -> return ()
                     'C':'H':_ -> do
                         card <- nextCM
                         case card of
                             CMTo v -> modify (currentPos .~ v)
                             CMBack v -> modify (currentPos %~ subtract v) >> toFixpoint
                             CMNext v -> modify (currentPos %~ (`cmNext` v))
                             CMNone -> return ()
                     _ -> return ()
         toFixpoint
    -- assume current position changed
    finPos <- gets (view currentPos)
    modify (stat %~ IM.insertWith (+) finPos 1)
  where

    pJail = nameToPos "JAIL"
    nextCC = do
        c <- gets (head . view ccPile)
        modify (ccPile %~ tail)
        return c
    nextCM = do
        c <- gets (head . view cmPile)
        modify (cmPile %~ tail)
        return c

main :: IO ()
main = do
    st <- view stat <$> execStateT (replicateM simulateN oneStep) initState
    let simulateResult = sortBy (flip compare `on` snd)
                       . map (\(pos,cnt) -> ((pos, posToName pos), toFreq cnt))
                       . IM.toList
                       $ st
    mapM_ prettyPrint simulateResult
  where
    prettyPrint ((pos,name),freq) = printf "%02d\t%s\t%.8f\n" pos name freq
    toFreq :: Int -> Double
    toFreq = (flip (/) `on` fromIntegral) simulateN
    simulateN = 1000000
