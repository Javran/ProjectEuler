import Control.Monad

main :: IO ()
main = do
    let p n = (n, n * (3*n - 1) `div` 2)
    let ps = map p [1..]
    let possibleDiffs :: [Int]
        possibleDiffs = do
          (nSum, pSum) <- ps
          let space = takeWhile ((< nSum).fst) ps
          (_, p2) <- space
          -- p2 + p1 = pSum
          let p1 = pSum - p2
          guard $ p1 <= p2 && any ((p1 ==).snd) space
          -- p2 - p1 = pDiff
          guard $ any (((p2-p1) ==).snd) space
          return $ p2 - p1

    print $ head possibleDiffs

