module Util.DeckIntegrity where

import HaskellHoldem.Dealer.Deck (Card(suit, value), Deck, Suit, Value)
import HaskellHoldem.Dealer.DeckActions (allFaceValues, allSuits)
import Test.Hspec

deckSize :: Int
deckSize = length allSuits * length allFaceValues

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

sumValue :: Value -> Deck -> Int
sumValue valueType = length . filter ((== valueType) . value)

isDup :: Deck -> Bool
isDup [] = False
isDup [_] = False
isDup (x:xs) = elem x xs || isDup xs

checkDeck :: String -> Deck -> Spec
checkDeck message deck =
    describe message $ do
        it "New deck has correct number of cards" $ do
            length deck `shouldBe` deckSize
        it "New deck has Correct number of suits" $ do
            map (`sumSuit` deck) allSuits `shouldBe` replicate (length allSuits) (length allFaceValues)
        it "New deck has Correct number of values" $ do
            map (`sumValue` deck) allFaceValues `shouldBe` replicate (length allFaceValues) (length allSuits)
        it "New Deck has no duplicates" $ do
            isDup deck `shouldBe` False
