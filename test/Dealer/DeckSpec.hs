module Dealer.DeckSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck (Card)
import Test.QuickCheck (quickCheck)
import Util.CardParser ()
import Util.DeckIntegrity (checkDeck)
import Util.QuickCheckInstances ()
import Test.Hspec (hspec)
import HaskellHoldem.Dealer.Deck (newDeck)

prop_card_in_deck :: Card -> Bool
prop_card_in_deck card = card `elem` newDeck

spec :: IO ()
spec = do
    putStrLn "REMOVED -- Deck QuickChecks"
    -- hspec $ checkDeck "Initial Deck Checks" newDeck
    -- quickCheck prop_card_in_deck
