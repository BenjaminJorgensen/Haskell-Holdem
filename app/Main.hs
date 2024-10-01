{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import System.Random.Stateful ( getStdGen, newIOGenM )
import HaskellHoldem.Dealer.DeckActions ( newDeck, shuffle, cardAction_, draw, drawM)
import Control.Monad (replicateM)
import HaskellHoldem.Dealer.DeckActions (cardAction)

-- Dealing Five hands of poker
main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    cards <- cardAction gen newDeck $ do
        cards <- replicateM 100000 drawM
        pure cards
    print cards

