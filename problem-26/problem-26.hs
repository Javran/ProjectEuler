import Data.List
import Data.Maybe
import Data.Function (on)

-- 1/n = 0.???? ...
-- this function finds ???? for n
--   by emulating how we do division on draft
--   (quotients and remainders are all kept,
--   so you might need `map fst $ fracPartList n`
fracPartList :: Int -> [(Int,Int)]
fracPartList n = unfoldr doDiv 10
    where 
        doDiv x
            | x   == 0  = Nothing 
            -- try to keep the first digit
            --   by assigning next state to be a dummy value(0)
            | rem == 0  = Just ((quo,rem),     0)
            | otherwise = Just ((quo,rem),rem*10)
                where (quo,rem) = x `divMod` n

cycleLen :: (Eq a) => [a] -> Int
cycleLen xs = cycleLenAux [] xs
    where
        cycleLenAux visited [] = 0
        cycleLenAux visited (hd:tl) =
            if hd `elem` visited
                then (fromJust $ elemIndex hd visited) + 1
                else cycleLenAux (hd:visited) tl

fracCycleLen :: Int -> Int
fracCycleLen n = cycleLen $ map snd $ fracPartList n

main = print $ maximumBy (compare `on` fracCycleLen) [2..999]
