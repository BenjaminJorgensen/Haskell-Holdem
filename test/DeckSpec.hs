module DeckSpec (spec) where
import Test.Hspec

spec :: Spec
spec = describe "Testing Cards" $ do
    it "Subtest for cards" $ do
        shouldBe "Cards" "Cards"
    it "Subtest for cards" $ do
        shouldBe "Cards" "Cards"
