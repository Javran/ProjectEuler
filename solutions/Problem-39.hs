import Control.Monad
import Data.List
import Data.Function (on)

solutions :: Int -> Int
solutions p = length sol
    where
        sol = do
            a <- [1..p]
            b <- [a..p]
            let c = p - a - b
            guard $ c*c == a*a + b*b
            return (a,b,c)

main :: IO ()
main = print $ maximumBy (compare `on` solutions) [1..1000 :: Int]
