module ProjectEuler.Problem54
  ( problem
  ) where

import Data.List
import Data.Char
import Data.Function

import qualified Data.Text as T

import ProjectEuler.Types

problem :: Problem
problem = pureProblemWithData "p54-poker.txt" 54 Solved compute

data Suit
  = C | S | H | D
  deriving (Eq, Show, Read)

data Card
  = Card
    { cValue :: Int
    , cSuit  :: Suit
    }
  deriving (Eq, Show)

allEqual :: (Eq e) => [e] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual [] = True

wordToCard :: String -> Card
wordToCard [] = error "unreachable"
wordToCard (v:s) = Card val (read s)
    where
      -- if we assign value 14 to 'A',
      -- we'll have:
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
lineToHand l = splitAt 5 $ wordToCard <$> words l

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
isIncConsecutive [] = error "unreachable"

-- rank a hand
handRank :: [Card] -> Int
handRank cs = fst $ head $ dropWhile (\(_, p) -> not p) $ zip [0..] rankingPredicates
  where
    -- search through predicates,
    --   find first true value,
    --   and use its index as rank
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
    cardValueCount  = sort . map length . group . sort $ map cValue cs

    -- Ten, Jack, Queen, King, Ace, in same suit.
    isRoyalFlush = isStraightFlush && any ((== 14) . cValue) cs
    -- All cards are consecutive values of same suit.
    isStraightFlush = isStraight && isFlush
    -- Four cards of the same value.
    isFourOfAKind  = cardValueCount == [1,4]
    -- Three of a kind and a pair.
    isFullHouse = cardValueCount == [2,3]
    -- All cards of the same suit.
    isFlush = allEqual $ map cSuit cs
    -- All cards are consecutive values.
    isStraight = isIncConsecutive $ sort $ map cValue cs
    -- Three cards of the same value.
    isThreeOfAKind = cardValueCount == [1,1,3]
    -- Two different pairs.
    isTwoPairs = cardValueCount == [1,2,2]
    -- Two cards of the same value.
    isOnePair = cardValueCount == [1,1,1,2]
    -- Highest value card.
    -- note: this is the fallback case, meaning it has to capture
    --   all cases that doesn't match any of above, therefore just `True`.
    isHighCard = True

compute :: T.Text -> Int
compute raw = length $ filter (uncurry firstWins) handPairs
  where
    handPairs = map lineToHand $ lines (T.unpack raw)


