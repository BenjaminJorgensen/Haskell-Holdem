module HaskellHoldem.Dealer.DeckActions where

import Control.Monad (forM, forM_)
import Control.Monad.Reader (MonadReader(ask), MonadTrans(lift), ReaderT(runReaderT))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState(get, put), StateT(runStateT), evalStateT)
import Data.Array.Base (elems, newListArray, readArray, writeArray)
import Data.Array.ST (STArray, runSTArray)
import qualified Data.Enum as DE
import HaskellHoldem.Dealer.Deck (Card(..), Deck, Suit, Value)
import System.Random (RandomGen)
import System.Random.Stateful (RandomGenM, StateGenM(StateGenM), StatefulGen, runStateGen, runStateGen_, uniformRM)

allSuits :: [Suit]
allSuits = [DE.minBound .. DE.maxBound]

allFaceValues :: [Value]
allFaceValues = [DE.minBound .. DE.maxBound]

-- CREATES A NEW CARD DECK
newDeck :: Deck
newDeck =
    [ Card {suit = s, value = v}
    | s <- [DE.minBound .. DE.maxBound] :: [Suit]
    , v <- [DE.minBound .. DE.maxBound] :: [Value]
    ]

-- * Monadic Deck Actions
-- Shuffles the deck using a variation on Fisher–Yates algorithm 
shuffle :: (StatefulGen g m) => Deck -> g -> m Deck
shuffle deck g = do
    let n = length deck
    rands <- forM [0 .. (n - 2)] $ \i -> uniformRM (i, n - 1) g
    pure
        $ elems
        $ runSTArray
        $ do
              arr <- newListArray (0, n - 1) deck :: ST s (STArray s Int Card)
              forM_ (zip [0 ..] rands) $ \(i, j) -> do
                  vi <- readArray arr i
                  vj <- readArray arr j
                  writeArray arr j vi
                  writeArray arr i vj
              return arr

-- Draws the topmost card from the deck and returns the new deck
draw :: (StatefulGen g m) => Deck -> g -> m (Card, Deck)
draw deck gen = do
    case deck of
        [] -> draw newDeck gen
        (x:xs) -> pure (x, xs)

-- * Mutateable Variants
-- | Shuffles with mutation
shuffleM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Deck
shuffleM = do
    gen <- ask
    deck <- get
    shuffled <- lift $ shuffle deck gen
    put shuffled >> pure shuffled

-- | Drawing with mutation
drawM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Card
drawM = do
    deck <- get
    case deck of
        [] -> shuffleM >> drawM
        (x:xs) -> put xs >> pure x

-- * Pure adapters
-- | A pure adapter shuffling a deck of cards
runShuffle :: (RandomGen g) => g -> Deck -> (Deck, g)
runShuffle gen deck = runStateGen gen (shuffle deck)

-- | A pure adapeter for doing a drawing a single card from a deck
runDraw :: (RandomGen g) => g -> Deck -> ((Card, Deck), g)
runDraw gen deck = runStateGen gen (draw deck)

-- | Returns the result of the card action and the state of the deck after the computation.
cardAction_ :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m (a, Deck)
cardAction_ gen deckState actions = runReaderT (runStateT actions deckState) gen

-- | Returns the result of the card action only
cardAction :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m a
cardAction gen deckState actions = runReaderT (evalStateT actions deckState) gen
