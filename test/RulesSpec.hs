module RulesSpec (spec) where
import Test.Hspec
import Deck
import Rules (evalFlush)

spec :: Spec
spec = do
        describe "Flush Rules" $ do
            it "Fails if no flush" $ do
                let cards = zipWith (\v s -> Card {value=v, suit=s}) [Two .. Eight] $ take 7 $ cycle allSuits
                evalFlush cards `shouldBe` Nothing
            it "Testing if Succeeds if flush exists" $ do
                let cardsLose = [Card {value=v, suit=s} | v <- [Two .. Eight], s <- [Diamonds]]
                let cardsWin = [Card {value=v, suit=s} | v <- (King):[Two .. Seven], s <- [Diamonds]]
                (evalFlush cardsWin) > (evalFlush cardsLose) `shouldBe` True
            it "Testing if Succeeds if flush exists 2" $ do
                let cardsLose = [Card {value=v, suit=s} | v <- [Queen .. Six], s <- [Diamonds]]
                let cardsWin = [Card {value=v, suit=s} | v <- (King):[Two .. Seven], s <- [Diamonds]]
                (evalFlush cardsWin) > (evalFlush cardsLose) `shouldBe` True
