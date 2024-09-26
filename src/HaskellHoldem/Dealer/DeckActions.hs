module HaskellHoldem.Dealer.DeckActions where

import Control.Monad (forM, forM_)
import Control.Monad.Reader (MonadReader(ask), MonadTrans(lift), ReaderT(runReaderT))
import Control.Monad.ST (ST)
import Control.Monad.State (MonadState(get, put), StateT(runStateT), evalStateT)
import Data.Array.Base (elems, newListArray, readArray, writeArray)
import Data.Array.ST (STArray, runSTArray)
import System.Random.Stateful (StatefulGen, uniformRM)
import HaskellHoldem.Dealer.Deck (Card(..), Deck, Suit, Value)
import qualified Data.Enum as DE

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



-- Shuffle Pure!
shuffle :: (StatefulGen g m) => g -> Deck -> m Deck
shuffle g deck = do
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

draw :: (StatefulGen g m) => g -> Deck -> m (Card, Deck)
draw gen deck = do
    case deck of
        [] -> draw gen newDeck
        (x:xs) -> pure (x, xs)

-- * Mutateable Variants
-- | Shuffles with mutation :)
--
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

runCardAction:: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m (a, Deck)
runCardAction gen deckState actions = runReaderT (runStateT actions deckState) gen

evalCardAction :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m a
evalCardAction gen deckState actions = runReaderT (evalStateT actions deckState) gen