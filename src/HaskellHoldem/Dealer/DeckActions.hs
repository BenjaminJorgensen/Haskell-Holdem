module HaskellHoldem.Dealer.DeckActions where

import Control.Monad (forM, forM_)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Control.Monad.ST (ST)
import Control.Monad.State.Strict (State)
import Data.Array.Base (elems, newListArray, readArray, writeArray)  
import Data.Array.ST (STArray, runSTArray)
import qualified Data.Enum as DE
import HaskellHoldem.Dealer.Deck (Card(..), Deck, Suit, Value)
import System.Random (RandomGen)
import System.Random.Stateful (StatefulGen, runStateGen, uniformRM, StateGenM, runStateGen_)
import Control.Monad.State.Lazy
    ( MonadTrans(lift),
      evalStateT,
      MonadState(put, get),
      StateT(runStateT))

allSuits :: [Suit]
allSuits = [DE.minBound .. DE.maxBound]

allFaceValues :: [Value]
allFaceValues = [DE.minBound .. DE.maxBound]

-- | Creates a new, unshuffled deck of cards.
--
-- The deck contains all possible combinations of 'Suit' and 'Value'.
--
-- ==== __Examples__
--
-- >>> take 5 newDeck
-- [♦2,♦3,♦4,♦5,♦6]
newDeck :: Deck
newDeck =
    [ Card {suit = s, value = v}
    | s <- [DE.minBound .. DE.maxBound] :: [Suit]
    , v <- [DE.minBound .. DE.maxBound] :: [Value]
    ]

--  * Monadic Deck Actions
--
-- | Shuffles the deck using a variation of the Fisher–Yates algorithm.
--
-- This function returns a new shuffled deck.
-- It requires a stateful random number generator.
-- It may be beneficial to use the 'runShuffle' adapter if attempting to use in
-- a pure context
--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> shuffledDeck <- shuffle newDeck gen
-- >>> take 5 $ shuffledDeck
-- [♠A,♦J,♠2,♠7,♣8]
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

-- | Draws the topmost card from the deck and returns the card along with the new deck.
--
-- If the deck is empty, a new deck is created and shuffled before drawing.
-- If attepting to run in a pure context, 'runDraw' may be more benificial
--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> let deck = newDeck
-- >>> (card, newDeck) <- draw deck gen
-- >>> card
-- >>> take 5 $ newDeck
-- ♦2
-- [♦3,♦4,♦5,♦6,♦7]
draw :: (StatefulGen g m) => Deck -> g -> m (Card, Deck)
draw deck gen = do
    case deck of
        [] -> draw newDeck gen
        (x:xs) -> pure (x, xs)

---- | A pure adapter for drawing a card from a deck or shuffling using a random generator.
----
---- It returns the result of the card action and the updated generator.
----
---- ==== __Examples__
----
---- >>> let ((card, deck), newGen) = cardAction draw (mkStdGen 100) newDeck
---- >>> newGen
---- >>> (card, take 5 $ deck)
---- StdGen {unStdGen = SMGen 16626775891238333538 2532601429470541125}
---- (♦2,[♦3,♦4,♦5,♦6,♦7])
cardAction :: RandomGen g => (Deck -> StateGenM g  -> State g a) -> g -> Deck -> (a, g)
cardAction function gen deck = runStateGen gen $ (function deck)

---- | A pure adapter for drawing a card from a deck or shuffling using a random generator.
----
---- It returns the result of the card action and discards updated generator.
----
---- ==== __Examples__
----
---- >>> let (card, deck) = cardAction_ draw (mkStdGen 100) newDeck
---- >>> (card, take 5 $ deck)
---- (♦2,[♦3,♦4,♦5,♦6,♦7])
cardAction_ :: RandomGen g => (Deck -> StateGenM g  -> State g a) -> g -> Deck -> a
cardAction_ function gen deck = runStateGen_ gen $ (function deck)



-- * Mutateable Deck actions

-- | Shuffles the deck in a mutable state (mutatable variant).
--
-- This function modifies the deck in place and can be used within a monad
-- that supports mutable state. Allows the shuffle to be run without worrying
-- about return values, states or generators, Must run with 'cardAction' or
-- similar functions.
--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> shuffledDeck <- cardAction gen newDeck $ do shuffleM
-- >>> take 5 $ shuffledDeck
-- [♠A,♦J,♠2,♠7,♣8]
shuffleM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Deck
shuffleM = do
    gen <- ask
    deck <- get
    shuffled <- lift $ shuffle deck gen
    put shuffled >> pure shuffled

-- | Draws a card from the deck in a mutable state (mutatable variant).
--
-- This function removes the topmost card from the deck and returns it, updating
-- the state of the deck. If the deck is empty, it shuffles a new deck before drawing.
-- Allows the draw to be run without worrying about return values, states or
-- generators. Must run with 'cardAction' or similar functions.

--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> card <- cardAction gen newDeck $ do drawM
-- >>> card
-- ♦2
drawM :: (MonadReader g m, StatefulGen g m) => StateT Deck m Card
drawM = do
    deck <- get
    case deck of
        [] -> do
            put newDeck
            shuffleM >> drawM
        (x:xs) -> put xs >> pure x

-- | Runs a card action (such as 'shuffleM' or 'drawM') on a deck and returns
-- both the result of the action and the updated deck.
-- This function provides a mutable context for deck manipulations using both
-- 'StateT' and 'ReaderT'. If desired, one can mutate the deck state from
-- within this context although it should be avoided whenever possilble.
--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> (result, deck) <- cardAction_ gen newDeck $ do
-- >>>     shuffleM
-- >>>     card <- drawM
-- >>>     pure card
-- >>> result
-- >>> take 5 $ deck
-- ♠A
-- [♦J,♠2,♠7,♣8,♠2]
cardActionM :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m (a, Deck)
cardActionM gen deckState actions = runReaderT (runStateT actions deckState) gen

-- | Runs a card action on a deck and returns only the result of the action.
--
-- The updated state of the deck is discarded.
--
-- ==== __Examples__
--
-- >>> gen <- newIOGenM (mkStdGen 100)
-- >>> result <- cardAction gen newDeck $ do
-- >>>     card <- drawM
-- >>>     pure card
-- >>> result
-- ♠A
cardActionM_ :: (StatefulGen g m) => g -> Deck -> StateT Deck (ReaderT g m) a -> m a
cardActionM_ gen deckState actions = runReaderT (evalStateT actions deckState) gen

