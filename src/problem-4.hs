main = print $ foldl max 0 $ filter isPalindromic [ x*y | x <-[999,998..100], y <-[x,(x-1)..100]]
    where isPalindromic x = strX == reverse strX
            where strX = show x
