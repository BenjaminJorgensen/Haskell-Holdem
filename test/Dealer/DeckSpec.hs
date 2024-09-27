module Dealer.DeckSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck (Card)
import Test.QuickCheck (quickCheck)
import Util.CardParser ()
import Util.DeckIntegrity (checkDeck)
import Util.QuickCheckInstances ()
import HaskellHoldem.Dealer.DeckActions (newDeck)
import Test.Hspec (hspec)

prop_card_in_deck :: Card -> Bool
prop_card_in_deck card = card `elem` newDeck

spec :: IO ()
spec = do
    putStrLn "Deck QuickChecks"
    hspec $ checkDeck "Initial Deck Checks" newDeck
    quickCheck prop_card_in_deck
