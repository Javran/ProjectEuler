import Control.Monad
import Data.List
import Data.Function (on)

solutions p = length sol
    where
        sol = do
            a <- [1..p]
            b <- [a..p]
            let c = p - a - b
            guard $ c*c == a*a + b*b
            return (a,b,c)

main = print $ maximumBy (compare `on` solutions) [1..1000]

{-
compile with: ghc problem-39.hs -O2
real    0m25.338s
user    0m24.536s
sys     0m0.700s
-}
