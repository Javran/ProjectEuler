-- https://oeis.org/A000984
main = print $ let fa x = if (x>1) then x * (fa (x-1)) else 1 in maximum [ fa(i+j) `div` ( (fa i) * (fa j)) | i<-[0..20], j<-[0..20]]
