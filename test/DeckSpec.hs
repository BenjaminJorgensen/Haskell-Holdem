module DeckSpec (spec) where
import Test.Hspec
import Deck

suits :: [Suit]
suits = [Diamonds, Hearts, Clubs, Spades]

spec :: Spec
spec = do
        describe "Deck Integrity" $ do
            it "New deck has correct number of cards" $ do
               (length newDeck) `shouldBe` (length suits)*13
            it "New deck has Correct number of suits" $ do
                map (`sumSuit` newDeck) suits `shouldBe` replicate (length suits) 13

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

