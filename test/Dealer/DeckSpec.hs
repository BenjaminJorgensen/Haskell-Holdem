module Dealer.DeckSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck
import HaskellHoldem.Dealer.DeckActions (newDeck)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)
import Util.CardParser ()
import Util.QuickCheckInstances ()

deckSize :: Int
deckSize = length allSuits * length allFaceValues

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

sumValue :: Value -> Deck -> Int
sumValue valueType = length . filter ((== valueType) . value)

prop_card_in_deck :: Card -> Bool
prop_card_in_deck card = card `elem` newDeck

isDup :: Deck -> Bool
isDup [] = False
isDup [_] = False
isDup (x:xs) = elem x xs || isDup xs

spec :: IO ()
spec = do
    putStrLn "Deck QuickChecks"
    quickCheck prop_card_in_deck
    hspec $ do
        describe "Deck LongChecks" $ do
            it "New deck has correct number of cards" $ do
                length newDeck `shouldBe` deckSize
            it "New deck has Correct number of suits" $ do
                map (`sumSuit` newDeck) allSuits `shouldBe` replicate (length allSuits) (length allFaceValues)
            it "New deck has Correct number of values" $ do
                map (`sumValue` newDeck) allFaceValues `shouldBe` replicate (length allFaceValues) (length allSuits)
            it "New Deck has no duplicates" $ do
                isDup newDeck `shouldBe` False
