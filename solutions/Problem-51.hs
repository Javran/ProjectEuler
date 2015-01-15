import Control.Monad 
import Data.Numbers.Primes
import Data.Char

-- generate a list that every element contains `1`
--   to save some conversion time, just keep it as Strings
contain1Nums :: [String]
contain1Nums = filter contains1 $ map show [0..]
    where
        contains1 = elem '1'

-- search space, we are finding primes,
--   so the last digit must be one of: 
--   [1,3,7,9]
searchSpace :: [String]
searchSpace = filter possiblePrime contain1Nums
    where
        possiblePrime = (`elem` "1379") . last

-- from a number that contains 1 to all posible masks
--   here is `1` plays the role of indicating that
--   this position is suitable of masking
--   a mask is a list of positions
-- e.g.: 3121
-- => [[1,3],[1],[3]]
oneNumToMask :: String -> [[Int]]
oneNumToMask xs = filter (not . null) $ powerset onePos
    where
        -- all position of ones
        --   all possible masks is just the non-empty powerset of this
        onePos = map fst $ filter ((== '1'). snd) $ zip [0..] xs

-- a trick to return the powerset of a list
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

answers :: [([Int],[Int])]
answers = do
    -- num that contains 1
    nStr <- searchSpace
    -- pick up one of the possible mask
    mask <- oneNumToMask nStr
    -- apply the mask to obtain the prime number family
    let family = filter isPrime $ applyMask nStr mask
    guard $ length family == 8
    return (family,mask)

-- apply the mask, get the family of 10 numbers
applyMask nStr mask = map doAll transRange 
    where
              -- 3. convert them back into integers
        doAll = read 
              -- 2. apply transformers so we get the family in their string form
              . (\t -> t indexedNStr)
              -- 1. make transformers from numbers
              . transformTo

        indexedNStr = zip [0..] nStr 
        transRange = if 0 `elem` mask
                        then [1..9]
                        else [0..9]
        -- transformTo takes a number `n`, and then an `indexedNStr`
        --   changes masked digits to `n` according to the setting of `mask`
        --   e.g.:
        --   zip [0..] "12311"
        --   transformTo 9 (zip [0..] "12311"),
        --                             ^  ^
        --                     mask = [0__3_]
        --   => "92391"
        transformTo :: Int -> [(Int,Char)] -> String
        transformTo n = map (\x ->
                if fst x `elem` mask
                    then chr $ n + ord '0'
                    else snd x)

main = print $ head answers
