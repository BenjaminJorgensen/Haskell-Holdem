{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import System.Random.Stateful
import System.Random
import Deck

-- Dealing Five hands of poker
main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    let deck = newDeck
    fresh <- shuffle gen deck
    print $ length fresh

