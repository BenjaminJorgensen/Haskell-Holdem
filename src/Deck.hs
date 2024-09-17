module Deck where
import qualified Data.Enum as DE
import Control.Monad.State
import System.Random.Stateful
import Control.Monad.Reader

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Ord, Bounded, Enum, Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Bounded, Ord, Enum, Read)
data Card = Card {suit :: Suit, value :: Value} deriving (Eq)
data Colour = Black | Red | Other deriving (Show, Eq)

type Deck = [Card]


allSuits :: [Suit]
allSuits = [DE.minBound .. DE.maxBound]

instance Ord Card where
    card `compare` other = value card `compare` value other

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

-- Shuffle helper 
shuffle :: StatefulGen g m => g -> Deck -> m Deck
shuffle = hShuffle [] 
    where
        hShuffle modDeck _ [] = pure modDeck
        hShuffle modDeck gen orig = do
            k <- uniformRM (0, length orig - 1) gen
            let (origL, origR) = splitAt k orig
            case origR of 
                [] -> pure modDeck
                (x:origRtail) -> hShuffle (x:modDeck) gen (origL ++ origRtail)

draw :: Deck -> Maybe (Card, Deck)
draw deck = case deck of
        [] -> Nothing
        (x:xs) -> Just (x, xs)

-- Shuffles with mutation
shuffleM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Deck
shuffleM = do
    gen <- ask
    deck <- get
    shuffled <- lift $ shuffle gen deck
    put shuffled >> pure shuffled

drawM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Card
drawM = do
    deck <- get
    case deck of
        [] -> shuffleM >> drawM
        (x:xs) -> put xs >> pure x


runCardState :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m (a, Deck)
runCardState gen deckState actions = runReaderT (runStateT actions deckState) gen

evalCardState :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m a
evalCardState gen deckState actions = runReaderT (evalStateT actions deckState) gen

