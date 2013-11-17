import ProjectEuler.Everything
import Control.Monad
import Data.Function (on)
import Data.List

-- enumerate through permutation should be more efficient
--   first thing is how we break the list (of 9 elements)
-- denote [xx,xx,xxx,xxx,xxx] to be [2,2,3,3,3] i.e. the length of each element
-- we want:
-- * sum [?,?,..,?] == 9
-- * [?,?,..,?] can only follow the pattern [a,a,..,a,b,b..,b] where b = a + 1
--   (because x*9 is at most one more digit than x)
-- * a*c + b*d == 9, c >= 1, d >= 0, b = a + 1, c + d > 1
--   => a*(c+d) == 9 - d
possibleSchemes :: [[Int]] 
possibleSchemes = do
    a <- [1..9]
    d <- [0..9]
    c <- [1..9]
    guard $ a * (c+d) == 9 - d
    guard $ c + d > 1
    return $ replicate c a ++ replicate d (a+1)

splitBy scheme xs = reverse $ fst $ foldl doSplit ([],xs) scheme
    where
        doSplit (done,todo) i = (hdPart:done, tlPart)
            where
                (hdPart, tlPart) = splitAt i todo

valid nums = all (uncurry (==)) $ zip nums [hd,hd*2..]
    where
        hd = head nums

main = do
    let solutions = do
        perm <- allPermutation [1..9]
        oneScheme <- map (flip splitBy perm) possibleSchemes
        let nums = map digitToNum oneScheme
        guard $ valid nums
        return $ nums
    
    print $ maximumBy (compare `on` head) solutions
{-
compile with: ghc -O2 problem-38.hs
real    0m1.169s
user    0m1.163s
sys     0m0.003s
-}
