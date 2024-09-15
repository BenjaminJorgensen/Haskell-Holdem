module Deck where
import qualified Data.Enum as DE

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show, Eq, Bounded, Enum, Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Bounded, Ord, Enum, Read)
data Card = Card {suit :: Suit, value :: Value}
data Colour = Black | Red | Other deriving (Show, Eq)
type Deck = [Card]

-- Gets the colour of a card
getColour :: Card -> Colour
getColour (Card Diamonds _) = Red
getColour (Card Hearts _) = Red
getColour (Card Spades _) = Black
getColour (Card Clubs _) = Black

-- CREATES A NEW CARD DECK
newDeck :: Deck
newDeck = [Card {suit=s, value=v} | 
    s <- [DE.minBound .. DE.maxBound] :: [Suit], 
    v <- [DE.minBound .. DE.maxBound] :: [Value]]
