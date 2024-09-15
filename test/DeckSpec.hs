module DeckSpec (spec) where
import Test.Hspec
import Deck

sumSuit :: Suit -> Deck -> Int
sumSuit suitType = length . filter ((== suitType) . suit)

-- True if list contains duplicate entries
isDup :: (Eq a) => [a] -> Bool
isDup [] = False
isDup (_:[]) = False
isDup (x:xs) = if elem x xs then True else isDup xs

suits :: [Suit]
suits = [Diamonds, Hearts, Clubs, Spades]

spec :: Spec
spec = do
        describe "Deck Integrity" $ do
            it "New deck has correct number of cards" $ do
               (length newDeck) `shouldBe` (length suits)*13
            it "New deck has Correct number of suits" $ do
                map (`sumSuit` newDeck) suits `shouldBe` replicate (length suits) 13
            it "Has no duplicates" $ do
                isDup newDeck `shouldBe` False

        describe "Deck Shuffling" $ do
            it "Deck shuffle changes card order" $ do
                ((shuffle newDeck) == newDeck) `shouldBe` False
            it "Deck shuffle does not remove cards" $ do
               (length (shuffle newDeck)) `shouldBe` (length suits)*13
            it "Deck shuffle doesn't change suits" $ do
                map (`sumSuit` (shuffle newDeck)) suits `shouldBe` replicate (length suits) 13
            it "Deck shuffle doesn't duplicate cards" $ do
                isDup (shuffle newDeck) `shouldBe` False
            it "Shuffled deck still has all original cards" $ do
                any (`elem` newDeck) (shuffle newDeck) `shouldBe` True

        describe "Drawing Cards" $ do
            it "Deck shuffle changes card order" $ do
                ((shuffle newDeck) == newDeck) `shouldBe` False
