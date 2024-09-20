{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import System.Random.Stateful ( getStdGen, newIOGenM )
import HaskellHoldem.Dealer.Deck ( newDeck, shuffle )

-- Dealing Five hands of poker
main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    let deck = newDeck
    fresh <- shuffle gen deck
    print $ length fresh

