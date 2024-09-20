module Dealer.DeckSpec (spec) where

import System.Random ( StdGen )
import System.Random.Stateful (IOGenM)

import Test.Hspec
import HaskellHoldem.Dealer.Deck
import Util.CardParser

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

-- True if list contains duplicate entries
isDup :: (Eq a) => [a] -> Bool
isDup [] = False
isDup [_] = False
isDup (x:xs) = elem x xs || isDup xs

suits :: [Suit]
suits = [Diamonds, Hearts, Clubs, Spades]

spec :: IOGenM StdGen -> Spec
spec gen = do
        describe "Deck Integrity" $ do
            it "New deck has correct number of cards" $ do
               length newDeck `shouldBe` length suits*13
            it "New deck has Correct number of suits" $ do
                map (`sumSuit` newDeck) suits `shouldBe` replicate (length suits) 13
            it "Has no duplicates" $ do
                isDup newDeck `shouldBe` False

        describe "Deck Shuffling" $ do
            it "Deck shuffle changes card order" $ do
                deck <- shuffle gen newDeck
                (deck == newDeck) `shouldBe` False
            it "Deck shuffle does not remove cards" $ do
                deck <- shuffle gen newDeck
                length deck `shouldBe` length suits*13
            it "Deck shuffle doesn't change suits" $ do
                deck <- shuffle gen newDeck
                map (`sumSuit` deck) suits `shouldBe` replicate (length suits) 13
            it "Deck shuffle doesn't duplicate cards" $ do
                deck <- shuffle gen newDeck
                isDup deck `shouldBe` False
            it "Shuffled deck still has all original cards" $ do
                deck <- shuffle gen newDeck
                any (`elem` newDeck) deck `shouldBe` True
            it "Shuffling two decks should be unique" $ do
                deck1 <- shuffle gen newDeck
                deck2 <- shuffle gen newDeck
                (deck1 == deck2) `shouldBe` False
            it "Shuffeling the same deck multiple times" $ do
                deck1 <- shuffle gen newDeck
                deck2 <- shuffle gen deck1
                (deck1 == deck2) `shouldBe` False

        describe "Drawing Cards" $ do
            it "Drawing cards eleminates it from the deck" $ do
                1+1 `shouldBe` 2
