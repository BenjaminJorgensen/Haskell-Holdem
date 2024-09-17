module RulesSpec (spec) where
import Test.Hspec
import Rules (evalFlush)
import CardParser (toCard)

spec :: Spec
spec = do
        describe "Flush Rules" $ do
            it "Fails if no flush" $ do
                let cards = toCard <$> ["2D", "3S", "4C", "5D", "6H", "10S", "KD"]
                evalFlush cards `shouldBe` Nothing
            it "Testing if Succeeds if flush exists" $ do
                let cardsLose = toCard <$>  ["2D", "3D", "4D", "5D", "6D", "7D", "8D"]
                let cardsWin = toCard <$>   ["3D", "4D", "5D", "6D", "7D", "8D", "9D"]
                (evalFlush cardsWin)  `shouldSatisfy` (> (evalFlush cardsLose))
            it "Testing if Succeeds if flush exists 2" $ do
                let cardsLose = toCard <$> ["4S", "4D", "5D", "6D", "7D", "8D", "9D"]
                let cardsWin = toCard <$>  ["KD", "3D", "4D", "5D", "6D", "7D", "8D"]
                (evalFlush cardsWin) `shouldSatisfy` (> (evalFlush cardsLose)) 
