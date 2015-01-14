import Data.Char

digitSum :: (Integral a, Show a) => a -> a
digitSum = sum . map ( fromIntegral . digitToInt ) . show

solve = maximum [ digitSum(a^b) | a<-[1..100], b<-[1..100]]

main = print solve
