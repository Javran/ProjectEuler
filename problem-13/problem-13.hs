import System.IO

modifyContents :: String -> String
modifyContents contents = take 10 $ show $ sum $ map read $ lines contents

main = do
	handle <- openFile "./numbers.txt" ReadMode
	contents <- hGetContents handle
	print $ modifyContents contents
	hClose handle
