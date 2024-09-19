{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module Deck where
import qualified Data.Enum as DE
import Control.Monad.State
import System.Random.Stateful
import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST
import Control.Monad
import Data.Array (Array)
import Data.Array.Base

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Ord, Bounded, Enum, Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Bounded, Ord, Enum, Read)
data Card = Card {suit :: Suit, value :: Value} deriving (Eq)
data Colour = Black | Red | Other deriving (Show, Eq)

type Deck = [Card]

allSuits :: [Suit]
allSuits = [DE.minBound .. DE.maxBound]

instance Ord Card where
    compare :: Card -> Card -> Ordering
    card `compare` other = value card `compare` value other

instance Show Card where
    show :: Card -> String
    show card = show (suit card) ++ show (value card) 

instance Show Suit where
    show :: Suit -> String
    show Diamonds = "♦"
    show Hearts =   "♥"
    show Spades =   "♠"
    show Clubs =    "♣"

instance Show Value where
    show :: Value -> String
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

-- Shuffle Pure!
shuffle :: (StatefulGen g m) => g -> Deck -> m Deck
shuffle g deck = do
    let n = length deck
    rands <- forM [0..(n-2)] $ \i -> uniformRM (i, n-1) g
    pure $ elems $ runSTArray $ do
        arr <- newListArray (0, n - 1) deck :: ST s (STArray s Int Card)
        forM_ (zip [0..] rands) $ \(i, j) -> do
            vi <- readArray arr i
            vj <- readArray arr j
            writeArray arr j vi
            writeArray arr i vj
        return arr



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

