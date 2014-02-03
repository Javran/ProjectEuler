
-- I think inductive way is faster.

-- T(n) = n(n+1)/2
-- T(n) = T(n-1) + n 
-- T(1) = 1
tSeq :: [Int]
tSeq = 1: zipWith (+) tSeq [2..]

-- P(n) = n(3n-1)/2
-- P(n) = P(n-1) + 3n-2
-- P(1) = 1
pSeq :: [Int]
pSeq = 1: zipWith op pSeq [2..]
    where acc `op` i = acc + 3 * i - 2

-- H(n) = n(2n-1)
-- H(n) = H(n-1) + 4n-3
-- H(1) = 1
hSeq :: [Int]
hSeq = 1: zipWith op hSeq [2..]
    where acc `op` i = acc + 4 * i - 3

-- look at the head of each seq from `seqS`
--   pick up the maximum,
--   drop all numbers smaller than it,
--   split heads and tails
dropSmalls :: [[Int]] -> [[Int]]
dropSmalls seqS = map (dropWhile (< theMax)) seqS
    where theMax = maximum $ map head seqS

seqEqual seqS =
    if allEqual seqHeads
        then seqHeads : seqEqual ( map tail seqS)
        else seqEqual $ dropSmalls seqS
    where
        seqHeads = map head seqS
        allEqual (x:xs) = all (== x) xs
  
main = print $ take 3 $ seqEqual [tSeq, pSeq, hSeq]
