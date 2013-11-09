{-
compile with: ghc problem-14.hs -rtsopts -O
run with: ./problem-14 +RTS -K200000000 -RTS

real	0m7.215s
user	0m6.960s
sys	    0m0.260s
-}

import qualified Data.Set as S

-- (set, (x,value))
-- set: 	a set to keep numbers that has been calculated
-- x: 		a number
-- value: 	the length that corresponding number, namely x has
type CalcStatus = ( S.Set Integer, (Integer, Int) )

collatz :: Integer -> Integer
collatz n = if odd n
	       then 3 * n + 1
	       else n `div` 2

generateCollatzList :: Integer -> [Integer]
generateCollatzList 1 = [1]
generateCollatzList x = x : (generateCollatzList $ collatz x)

-- returns (x, value) in which value should be the biggest one in terms of its collatz length
solve :: Integer -> CalcStatus
solve limit = foldl doCalc (S.fromList [1], (1, 1) ) $ reverse [1 .. limit] where
	doCalc :: CalcStatus -> Integer -> CalcStatus
	doCalc (s, (maxX, maxLen)) i = result 
		where
			result = if (newList == []) 
				then 
				-- there is nothing new
					(s, (maxX, maxLen))
				else 
				-- insert new elements into the list and update (x, value) if it is possible	
					(newS, (newMaxX, newMaxLen))
			newGenList = generateCollatzList i
			newList = takeWhile (\e -> not $ e `S.member` s) newGenList
			newS = foldr S.insert s newList 
			(newMaxX, newMaxLen) = if ( length newGenList > maxLen )
				then (i, length newGenList)
				else (maxX, maxLen)

main = print $ snd $ solve 1000000
