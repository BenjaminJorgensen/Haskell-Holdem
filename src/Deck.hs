module Deck where
import qualified Data.Enum as DE
import System.Random
import Control.Monad.State
import System.Random.Stateful (StatefulGen)

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Bounded, Enum, Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Bounded, Ord, Enum, Read)
data Card = Card {suit :: Suit, value :: Value} deriving (Eq)
data Colour = Black | Red | Other deriving (Show, Eq)
type Deck = [Card]

instance Show Card where
    show card = show (suit card) ++ show (value card) 

instance Show Suit where
    show Diamonds = "♦"
    show Hearts =   "♥"
    show Spades =   "♠"
    show Clubs =    "♣"

instance Show Value where
    show Ace =      "A"
    show King =     "K"
    show Queen =    "Q"
    show Jack =     "J"
    show v = show $ fromEnum v + 2

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

-- SHUFFLES THE GIVEN DECK
shuffle :: Deck -> State StdGen (Deck)
shuffle = undefined

-- randomSt :: (RandomGen g, Random a) => State g a
-- randomSt = state random

-- threeCoins :: (Int,Int,Int) -> State StdGen (Int,Int,Int)
threeCoins :: StatefulGen g m => (Int,Int,Int) -> g -> m (Int,Int,Int)
threeCoins (x,y,z) = do
    a <- random
    b <- random
    c <- random
    return (a+x,b+y,c+z)

-- DRAWS A CARD FROM THE DECK, REMOVING IT,
-- FAILS IF DECK IS EMPTY
draw :: Deck -> Maybe Deck
draw = undefined
