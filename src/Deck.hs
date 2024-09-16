module Deck where
import qualified Data.Enum as DE
import Control.Monad.State
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))

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
getColour card 
        | suit card `elem` [Diamonds, Hearts] = Red
        | suit card `elem` [Spades, Clubs] = Black
        | otherwise = Other

-- CREATES A NEW CARD DECK
newDeck :: Deck
newDeck = [Card {suit=s, value=v} | 
    s <- [DE.minBound .. DE.maxBound] :: [Suit], 
    v <- [DE.minBound .. DE.maxBound] :: [Value]]

-- TODO:
-- Implement a O(n) algorithm for shuffling using

-- To initialize an array a of n elements to a randomly shuffled copy of source, both 0-based:
--   for i from 0 to n − 1 do
--       j ← random integer such that 0 ≤ j ≤ i
--       if j ≠ i
--           a[i] ← a[j]
--       a[j] ← source[i]

-- Original Fisher and Yates' method for shuffling even though faster
-- algorithms exist (https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
--
-- SHUFFLES THE GIVEN DECK
shuffle :: StatefulGen g m => Deck -> g -> m Deck
shuffle deck gen = shuffle' deck [] gen
    where
        shuffle' :: StatefulGen g m => Deck -> Deck -> g -> m Deck
        shuffle' [] modDeck _ = pure modDeck
        shuffle' orig modDeck gen' = do
            k <- uniformRM (0, length orig - 1) gen'
            let (origL, origR) = splitAt k orig
            case origR of 
                [] -> pure modDeck
                (x:origRtail) -> shuffle' (origL ++ origRtail) (x:modDeck) gen'


-- DRAWS A CARD FROM THE DECK, REMOVING IT,
-- FAILS IF DECK IS EMPTY
draw :: State Deck Card
draw = state $ \deck -> case deck of
    []      -> error "Pulled from an empty deck"
    (x:xs)  -> (x, xs)

