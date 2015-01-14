import Control.Monad

main = do
    let p n = (n, n * (3*n - 1) `div` 2)
    let ps = map p [1..]
    let possibleDiffs = do
        (nSum, pSum) <- ps
        let space = reverse $ takeWhile ((< nSum).fst) ps
        (n2, p2) <- space
        -- p2 + p1 = pSum
        let p1 = pSum - p2
        guard $ p1 <= p2
        guard $ any ((p1 ==).snd) space
        -- p2 - p1 = pDiff
        guard $ any (((p2-p1) ==).snd) space
        return $ p2 - p1

    print $ head possibleDiffs
{-

real    0m9.569s
user    0m9.573s
sys     0m0.000s

-}
