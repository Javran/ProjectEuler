import System.IO

modifyContents :: String -> Int
modifyContents content = maximum $ map (takeProduct ( map (stringsToInts.words) $ lines content )) ( 
	orderAccessListX ++ orderAccessListY ++ orderAccessListA ++ orderAccessListB)

stringsToInts :: [String] -> [Int]
stringsToInts = map read

breakInto4 :: [Int] -> [[Int]]
breakInto4 (a:b:c:d:xs) = filter ((/= 0) . length) $ [a,b,c,d] : breakInto4 (b:c:d:xs)
breakInto4 xs = []


-- let order be: height, weight (x, y)
-- range: [0 .. maxrange-1]


-- given x, y from 0 to (weight-1)
generateLineX :: Int -> Int -> [[ (Int, Int) ]]
generateLineX x weight = takeWhile acceptList [ generateFromXStart x yIter | yIter <- [0..] ] where
	acceptList =  all ((<= (weight-1)).snd)
	generateFromXStart x start = [ (x,y) | y <- [start .. start+3] ]

orderAccessListX :: [[ (Int, Int) ]]
orderAccessListX = concat [generateLineX x 20 | x <- [0..19]]

orderAccessListY :: [[ (Int, Int) ]]
orderAccessListY = map (map (\(a,b)->(b,a))) orderAccessListX

-- this list will take elements diagonally ( from left-top to right-bottom )
orderAccessListA :: [[ (Int, Int) ]]
orderAccessListA = filter allowItem $ map transTo4List [ (x,y) | x <- [0..19] , y <- [0..20]] where
	allowItem = all allowPair 
	allowPair (x,y) = x `elem` [0..19] && y `elem` [0..19]
	transTo4List (x,y) = map (pairAdd (x,y)) [0..3]
	pairAdd (x,y) n = (x+n, y+n)

orderAccessListB :: [[ (Int, Int) ]]
orderAccessListB = map mirrorListCoor orderAccessListA where
	mirrorListCoor = map mirrorCoor
	mirrorCoor (x,y) = (x, 19-y)

-- take 4 coor and returns the product of them
takeProduct :: [[Int]] -> [(Int, Int)] -> Int
takeProduct table [a,b,c,d] = product $ map toValue [a,b,c,d] where
	toValue (x,y) = table !! x !! y

main = do
	handle <- openFile "./grid.txt" ReadMode
	contents <- hGetContents handle
	print $ modifyContents contents
	hClose handle
