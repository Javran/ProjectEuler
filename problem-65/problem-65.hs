import Data.Ratio

eExpand :: [Integer]
eExpand = 2 : concatMap (\x -> [1,2*x,1]) [1..]

approx :: [Integer] -> Rational
approx [] = error "invalid input"
approx [x] = x%1
approx (x:xs) = (x%1) + (1%1) / approx xs

main :: IO ()
main = print
     . (sum :: [Int] -> Int)
     . map (read . (:[])) . show
     . numerator
     . approx
     $ take 100 eExpand

