module Main where
import System.Random
import System.Random.Stateful (newIOGenM)
import Deck (Deck, draw, shuffle, newDeck)
import Control.Monad.State
import Control.Monad (replicateM)

-- TODO: TESTING FUNCTION THIS
dealHeand :: State Deck Deck
dealHeand = replicateM 2 draw

-- Dealing Five hands of poker
main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    deck <- shuffle newDeck gen
    let (hands, deck') =  runState (replicateM 5 dealHeand) deck
    let (pile, _) =  runState (replicateM 3 draw) deck'
    print pile
    print hands
