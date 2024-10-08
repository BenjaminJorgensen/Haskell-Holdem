module HaskellHoldem.Dealer.Judgement (evalFlush, isFlush) where
import HaskellHoldem.Dealer.Deck
import HaskellHoldem.Dealer.DeckActions
import Data.List
-- Defining logic that applies to the game as a whole

-- type Hand = (Card, Card)
-- type Rank = Int
-- type Score a = (Rank, a)

-- UTILITIES
countSuit :: [Card] -> [(Int, Suit)]
countSuit cards = map (\x -> (length (filter (== x) $ map getSuit cards), x)) allSuits

-- Gives a numerical value to the hand based on poker rules
-- evalHand :: [Card] -> Score a
-- evalHand cards = undefined

-- EVALUATE FLUSH
-- A flush is a hand that contains five cards all of the same suit
-- Give score if flush exists
evalFlush :: [Card] -> Maybe [Int]
evalFlush cards = do
    validCards <- isFlush cards
    pure $ reverse $ sort $ fromEnum . getValue <$> validCards

isFlush :: [Card] -> Maybe [Card]
isFlush cards = 
    let (count, suitType) = maximum $ countSuit cards
    in if count < 5 
       then Nothing 
       else Just (filter (\card -> getSuit card == suitType) cards)
