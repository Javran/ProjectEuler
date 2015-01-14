import Data.Char
import Data.List

fibSeq :: [(Integer, Integer)]
fibSeq = map (\(n, (a,_)) -> (n, a)) $ iterate (\(n, (a,b))->(n+1, (b,a+b))) (1,(1,1))

fstPandigital :: Integer -> Bool
fstPandigital n = charNum == "123456789" where
    charNum = sort $ take 9 $ show n

lstPandigital :: Integer -> Bool
lstPandigital n = charNum == "123456789" where
    charNum = sort $ show $ n `mod` 1000000000

result = fst $ head $ filter (\(n, x)-> lstPandigital x && fstPandigital x ) fibSeq

main = print result

{-
compile with: ghc -O2 problem-104.hs -rtsopts
run with: ./problem-104 +RTS -K20000000 -RTS

real    0m3.166s
user    0m3.160s
sys     0m0.007s
-}
