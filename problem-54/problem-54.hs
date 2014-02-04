-- hGetContents is too lazy
import System.IO hiding (hGetContents)
import qualified System.IO.Strict as S
import Control.Monad
import Control.DeepSeq
import Data.List
import Data.Char
import Data.Function

data Suit = C | S | H | D
    deriving (Eq, Show, Read)

data Card = Card 
    { cValue :: Int
    , cSuit  :: Suit
    } deriving (Eq, Show)

allEqual :: (Eq e) => [e] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual []     = True

wordToCard :: String -> Card
wordToCard (v:s) = Card val (read s) 
    where
        -- make 'A' being 14, so that:
        --  card1 >= card2 iff. v1 >= v2
        val = case v of
            'T' -> 10
            'J' -> 11
            'Q' -> 12
            'K' -> 13
            'A' -> 14
            _   -> ord v - ord '0'

-- a raw string line to a hand
lineToHand :: String -> ([Card],[Card])
lineToHand l = splitAt 5 $ map wordToCard $ words l 

-- the situation when the first player wins
firstWins :: [Card] -> [Card] -> Bool
firstWins p1Cards p2Cards = case (compare `on` handRank) p1Cards p2Cards of
    -- p1 rank > p2 rank, p2 wins
    GT -> False
    -- p1 rank < p2 rank, p1 wins
    LT -> True
    -- tie on rank
    --   break the tie: e.g. [2,2,4,4,4], [3,3,3,9,9] => p1 should win
    EQ -> p1Cards `greaterThanByValue` p2Cards
            where greaterThanByValue = (>) `on` cardsToFreqGroup
                  cardsToFreqGroup = map fst
                                   -- step 2: sort by freq and value
                                   --   if freq ties, break tie by using the value
                                   --   e.g. [2,2,3,3,9]
                                   --     => [(2,2),(3,2),(9,1)]
                                   --     => [(3,2),(2,2),(9,1)]
                                   . sortBy (flip compare `on` \(x,y) -> y * 1000 + x)
                                   -- step 1: e.g. [2,2,4,4,4] => [(4,3),(2,2)]
                                   . map (\ l@(x:_) -> (x,length l))
                                   . group . sort
                                   . map cValue

-- the list given is increasing and consecutive
isIncConsecutive :: [Int] -> Bool
isIncConsecutive l@(x:_) = and $ zipWith (==) l [x..]

-- rank a hand
handRank :: [Card] -> Int
handRank cs = fst $ head $ dropWhile (\(_, p) -> not $ p cs) $ zip [0..] rankingPredicates
    where
        -- search through predicates,
        --   find first predicate that returns True
        --   and use its index as rank
        rankingPredicates :: [[Card] -> Bool] 
        -- note: the lowest number stands for the highest rank
        rankingPredicates =
            [ isRoyalFlush 
            , isStraightFlush
            , isFourOfAKind
            , isFullHouse
            , isFlush
            , isStraight
            , isThreeOfAKind
            , isTwoPairs
            , isOnePair
            , isHighCard
            ]

        -- get the card values
        --   and group cards of the same value
        --   return the size of each group, the return value is already sorted
        --   e.g. [1,2,3,4,4] => [1,1,1,2]
        --        [5,5,6,5,6] => [2,3]
        cardValueCount  cs = sort $ map length $ group $ sort $ map cValue cs

        -- Highest value card.
        isHighCard      cs = True

        -- Two cards of the same value.
        isOnePair       cs = cardValueCount cs == [1,1,1,2]

        -- Two different pairs.
        isTwoPairs      cs = cardValueCount cs == [1,2,2]

        -- Three cards of the same value.
        isThreeOfAKind  cs = cardValueCount cs == [1,1,3]

        -- All cards are consecutive values.
        isStraight      cs = isIncConsecutive $ sort $ map cValue cs

        -- All cards of the same suit.
        isFlush         cs = allEqual $ map cSuit cs

        -- Three of a kind and a pair.
        isFullHouse     cs = cardValueCount cs == [2,3]

        -- Four cards of the same value.
        isFourOfAKind   cs = cardValueCount cs == [1,4]

        -- All cards are consecutive values of same suit.
        isStraightFlush cs = isStraight cs && isFlush cs

        -- Ten, Jack, Queen, King, Ace, in same suit.
        isRoyalFlush    cs = isStraightFlush cs && any ((== 14) . cValue) cs

handleFile :: Handle -> IO Int
handleFile h = do
    contents <- S.hGetContents h
    let handPairs = map lineToHand $ lines contents
    return $ length $ filter (uncurry firstWins) handPairs
    
main = withFile "./poker.txt" ReadMode handleFile
   >>= print
