module CardParserSpec (spec) where
import Test.Hspec
import Deck
import CardParser

spec :: Spec
spec = do
        describe "Testing Parser" $ do
            it "Diamonds" $ do
                let card = toCard "2D"
                card `shouldBe` Card {value=Two, suit=Diamonds}
            it "Spades" $ do
                let card = toCard "KS"
                card `shouldBe` Card {value=King, suit=Spades}
            it "Hearts" $ do
                let card = toCard "AH"
                card `shouldBe` Card {value=Ace, suit=Hearts}
            it "Clubs" $ do
                let card = toCard "10C"
                card `shouldBe` Card {value=Ten, suit=Clubs}
