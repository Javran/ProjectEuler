sel n r = (product [n-r+1..n]) `div` (product [1..r])
main = print $ sum [1 | n <- [1..100], r<-[0..n],(sel n r)>1000000 ]
