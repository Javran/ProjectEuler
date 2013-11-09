import System.IO
import Data.Ix

-- let's do a line-by-line fold
--   acc = first line = [a1], i = next line, [a2, a3]
--   a2 -> examine positiion in acc: -1 and 0
--   a3 -> examine positiion in acc: 0 and 1
solveMax :: [[Int]] -> Int
solveMax (t:ts) = foldl max 0 bottomLine
    where
        bottomLine = foldl solveNextLine t ts
        solveNextLine curLine nextTableLine = map possibleMax nextTableLinePos
            where
                nextTableLinePos = zip [0..] nextTableLine
                possibleMax (pos,val) = val + (foldl max 0 possibleVal)
                    where
                        possiblePos = filter valid [pos - 1, pos]
                        possibleVal = map (curLine !!) possiblePos
                        valid = inRange (0, (length curLine)-1)

main :: IO ()
main = do
    h <- openFile "./triangle.txt" ReadMode
    content <- hGetContents h
    let table = map ( (map read) . words) $ lines content :: [[Int]]

    print $ solveMax table
