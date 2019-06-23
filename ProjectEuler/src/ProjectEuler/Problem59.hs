module ProjectEuler.Problem59
  ( problem
  ) where

import Data.Word
import Data.List.Split
import Data.List
import Control.Arrow
import Data.Function
import Data.Char
import Data.Bits

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p59-cipher.txt" 59 Solved compute

getCipherText :: T.Text -> [Word8]
getCipherText raw =
  read $ "[" ++ T.unpack raw ++ "]"

getFreq :: [Word8] -> [(Word8, Int)]
getFreq = sortBy (flip compare `on` snd) . map (head &&& length) . group . sort

guessKey :: [(Word8, Int)] -> Word8
-- hmm... the most frequent letter might be spaces
guessKey = (`xor` fromIntegral (ord ' '))  . fst . head

-- for XOR encryptions, encryption is the same as decryption
encrypt :: [Word8] -> [Word8] -> [Word8]
encrypt key = zipWith xor (cycle key)

compute :: T.Text -> Int
compute raw = sum $ fromIntegral <$> plaintext
  where
    cipher = getCipherText raw
    -- key is three lowercase letters
    -- we divide ciphertext into 3 chunks
    -- and do frequence analysis to see if that helps
    cipherGroups = transpose $ chunksOf 3 cipher
    key = map (guessKey . getFreq) cipherGroups
    plaintext :: [Word8]
    plaintext = encrypt key cipher
