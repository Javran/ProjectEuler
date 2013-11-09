-- case of 1 to 9
-- define digitToLen 0 = 0 for convenience
digitToLen :: Int -> Int
digitToLen x
	| x < 0 = error "Unsupported"
	| x > 9 = error "Unsupported"
	| x == 0 = 0
	| otherwise = length ( ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (x-1))

-- case of 10 to 19
numToLen1x :: Int -> Int
numToLen1x x
	| lowDigit < 0 = error "Unsupported"
	| lowDigit > 9 = error "Unsupported"
	| otherwise = length (wordList !! lowDigit)	-- 0<= lowDigit <= 9
	where lowDigit = x `mod` 10
	      wordList = ["ten" , "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

-- case of [20,30..90]
numToLenx0 :: Int -> Int
numToLenx0 x
	| highDigit < 2 = error "Unsupported"
	| highDigit > 9 = error "Unsupported"
	| otherwise = length (wordList !! (highDigit-2) )	-- 2<= highDigit <= 9
	where highDigit = x `div` 10
	      wordList = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- case of [1..99]
numToLenxy x
	| x == 0 = 0 
	| x < 1 = error "Unsupported"
	| x > 99 = error "Unsupported"
	| x <= 9 = digitToLen x -- case 1 to 9
	| x <= 19 = numToLen1x x -- case 10 to 19
	| otherwise = (numToLenx0 x) + (digitToLen lowDigit) -- case 20-99
	where highDigit = x `div` 10
	      lowDigit = x `mod` 10

numToLen :: Int -> Int
numToLen x
	| x < 1 = error "Unsupported"
	| x > 1000 = error "Unsupported"
	| x == 1000 = length "onethousand"
	| x < 100 = numToLenxy x
	| otherwise = (digitToLen high1Digit) + length "hundred" + (if low2Digit == 0 then 0 else length "and") + (numToLenxy low2Digit)
	where high1Digit = x `div` 100
	      low2Digit = x `mod` 100

main = print $ sum $ map numToLen [1..1000]
