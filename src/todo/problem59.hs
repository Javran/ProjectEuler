import Data.Word
import Data.List.Split
import Data.List
import Control.Arrow
import Data.Function
import Data.Char
import Data.Bits
import Text.Printf

getCipherText :: IO [Word8]
getCipherText = do
    contents <- readFile "p059_cipher.txt"
    return $ read $ "[" ++ contents ++ "]"

getFreq :: [Word8] -> [(Word8, Int)]
getFreq = sortBy (flip compare `on` snd) . map (head &&& length) . group . sort

guessKey :: [(Word8, Int)] -> Word8
-- hmm... the most frequent letter might be spaces
guessKey = (`xor` fromIntegral (ord ' '))  . fst . head

-- for XOR encryptions, encryption is the same as decryption
encrypt :: [Word8] -> [Word8] -> [Word8]
encrypt key = zipWith xor (cycle key)

word2Char :: Word8 -> Char
word2Char = chr . fromIntegral

main :: IO ()
main = do
    cipher <- getCipherText
    -- key is three lowercase letters
    -- we divide ciphertext into 3 chunks
    -- and do frequence analysis to see if that helps
    let cipherGroups = transpose $ chunksOf 3 cipher
        key = map (guessKey . getFreq) cipherGroups
        plaintext = encrypt key cipher

    printf "I guess the key is '%s'.\n" (map word2Char key)
    putStrLn "Plaintext:"
    putStrLn $ map word2Char plaintext
    printf "Answer to the problem: %d.\n" (sum (map fromIntegral plaintext) :: Int)
