import Data.List
import Data.Char
import Control.Monad

main :: IO ()
main = do
    let inCommon a b = show a `intersect` show b
        removeCommon a b = (a1,b1)
            where
                c = inCommon a b
                a1 = ord (head $ show a \\ c) - ord '0'
                b1 = ord (head $ show b \\ c) - ord '0'

        canCancel a b = length (inCommon a b) == 1 && a1 * b == a * b1
            where
                (a1,b1) = removeCommon a b
        searchSpace = do
            a <- [10..99]
            b <- [a..99]
            guard $ a < b
            guard $ canCancel a b
            guard $ inCommon a b /= "0"
            return (a,b)
        simplify (a,b) = (a `div` g, b `div` g)
            where g = gcd a b

    print $ simplify $ foldl (\(a,b) (c,d) -> (a*c,b*d)) (1,1) searchSpace
