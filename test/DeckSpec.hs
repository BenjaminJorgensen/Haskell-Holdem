module DeckSpec (spec) where
import System.Random
import System.Random.Stateful (IOGenM)
import Control.Monad.State

import Test.Hspec
import Deck
import Control.Monad (replicateM)

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

-- True if list contains duplicate entries
isDup :: (Eq a) => [a] -> Bool
isDup [] = False
isDup (_:[]) = False
isDup (x:xs) = if elem x xs then True else isDup xs

suits :: [Suit]
suits = [Diamonds, Hearts, Clubs, Spades]

spec :: IOGenM StdGen -> Spec
spec gen = do
        describe "Deck Integrity" $ do
            it "New deck has correct number of cards" $ do
               (length newDeck) `shouldBe` (length suits)*13
            it "New deck has Correct number of suits" $ do
                map (`sumSuit` newDeck) suits `shouldBe` replicate (length suits) 13
            it "Has no duplicates" $ do
                isDup newDeck `shouldBe` False

        describe "Deck Shuffling" $ do
            it "Deck shuffle changes card order" $ do
                deck <- shuffle newDeck gen
                (deck == newDeck) `shouldBe` False
            it "Deck shuffle does not remove cards" $ do
                deck <- shuffle newDeck gen
                (length deck) `shouldBe` (length suits)*13
            it "Deck shuffle doesn't change suits" $ do
                deck <- shuffle newDeck gen
                map (`sumSuit` deck) suits `shouldBe` replicate (length suits) 13
            it "Deck shuffle doesn't duplicate cards" $ do
                deck <- shuffle newDeck gen
                isDup deck `shouldBe` False
            it "Shuffled deck still has all original cards" $ do
                deck <- shuffle newDeck gen
                any (`elem` newDeck) deck `shouldBe` True
            it "Shuffling two decks should be unique" $ do
                deck1 <- shuffle newDeck gen
                deck2 <- shuffle newDeck gen
                (deck1 == deck2) `shouldBe` False
            it "Shuffeling the same deck multiple times" $ do
                deck1 <- shuffle newDeck gen
                deck2 <- shuffle deck1 gen
                (deck1 == deck2) `shouldBe` False

        describe "Drawing Cards" $ do
            it "Drawing cards eleminates it from the deck" $ do
                deck <- shuffle newDeck gen
                let (card, pile) = runState draw deck
                (elem card pile) `shouldBe` False
                (elem card deck) `shouldBe` True
            it "Drawing from an unshuffled deck" $ do
                let card = evalState draw newDeck
                suit card `shouldBe` Diamonds
                value card `shouldBe` Two
            it "Draw Three cards" $ do
                let cards = evalState (replicateM 3 draw) newDeck
                length cards `shouldBe` 3
                all (\c -> suit c == Diamonds) cards `shouldBe` True
                (value <$> cards) == [Two,Three,Four] `shouldBe` True
            it "Draw Three cards - Shuffled" $ do
                shuffled <- shuffle newDeck gen
                let cards = evalState (replicateM 3 draw) shuffled
                length cards `shouldBe` 3
                all (\c -> suit c == Diamonds) cards `shouldBe` False
                (value <$> cards) == [Two,Three,Four] `shouldBe` False
