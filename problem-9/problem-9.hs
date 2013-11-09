(x,y) = head $ [(a,b) | a<-[0..1000], b<-[a+1..1000],  a<b, b < (1000-a-b), a * a + b * b == (1000-a-b) * (1000-a-b)]

main = print $ x * y * (1000 - x - y)
