{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

-- import System.Random.Stateful ( getStdGen, newIOGenM )
-- import HaskellHoldem.Dealer.DeckActions ( newDeck, shuffle, cardAction_, draw, drawM)
-- import Control.Monad (replicateM)
-- import HaskellHoldem.Dealer.DeckActions (cardAction)
import HaskellHoldem.Dealer.Judgement 
import Util.CardParser (toCard)
import Data.Word (Word32)
import HaskellHoldem.Dealer.Deck (newDeck)

cardText :: [String]
cardText = [
        "2D",
        "3D",
        "4D",
        "5D",
        "6C",
        "7D",
        "8D",
        "9D",
        "10D",
        "JS",
        "QS",
        "KS",
        "AS"]


-- Dealing Five hands of poker
main :: IO ()
main = do
    let cards = toCard <$> cardText
    print cards
    print newDeck
