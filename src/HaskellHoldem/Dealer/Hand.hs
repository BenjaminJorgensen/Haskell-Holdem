module HaskellHoldem.Dealer.Hand where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy)
import Data.Word (Word16, Word32)
import HaskellHoldem.Dealer.Deck ( Card, getValue, getSuit )

data Hand = Hand
    { quint :: Word32
    , suitBits :: [(Int, Word16)]
    }

-- PERF: Find faster way to constructHand's
constructHand :: [Card] -> Hand
constructHand cards = Hand {quint = calculateQuint, suitBits = makeSuitBits cards}
  where
    calculateQuint = sum [5 ^ (getSuit card) | card <- cards]
    makeSuitBits = fmap (length &&& createBits) . groupBy ((==) `on` getSuit)
    createBits cardsInSuit = sum [2 ^ (getValue card) | card <- cardsInSuit]
