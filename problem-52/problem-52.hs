import Data.List

allEqual :: (Eq e) => [e] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual []     = True

isPermNum :: Int -> Bool
isPermNum n = allEqual $ map ( sort . show . (* n') ) [1..6]
    where n' = oneInFront n

-- the number have to start with `1`
oneInFront :: Int -> Int
oneInFront n = read $ '1' : show n

main = print $ oneInFront $ head $ filter isPermNum [1..]
