nfibs n =let r = replicate (n-1) 0 ++ 1 : 1 : zipWith ((-).(2*)) (drop n r) r in r

fib x = nfibs 2 !! x

main =
    print $ head [x | x <-[4751..], (length . show . fib) x >=1000 ]
