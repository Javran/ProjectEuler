powerMod :: (Integral a) => a -> a -> a -> a
powerMod x y m = powerModAux x y m 1
    where
        -- (c * x^y) mod m is a constant
        powerModAux x y m c
            | y == 0  = c `mod` m
            | odd y   = (powerModAux x (y-1) m (c*x)) `mod` m
            | even y  = (powerModAux (x*x) (y `div` 2) m c) `mod` m

limit = 10000000000

main = print $ (sum $ map (\x -> powerMod x x limit) [1..1000]) `mod` limit
