-- https://oeis.org/A000984
main :: IO ()
main = print (maximum [ fa(i+j) `div` (fa i * fa j)
                      | i<- [0..20 :: Int]
                      , j<- [0..20]])
  where
    fa x = if x > 0 then x * fa (x-1)
                    else 1
