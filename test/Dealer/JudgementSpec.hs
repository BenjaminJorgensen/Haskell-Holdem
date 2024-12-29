module Dealer.JudgementSpec (spec) where
import Test.Hspec ( describe, it, shouldSatisfy, hspec )
import HaskellHoldem.Dealer.Judgement ( evalFlush ) 
import Util.CardParser ( toCard )

spec :: IO ()
spec = hspec $ do
        describe "Evaluation Flush" $ do
            it "Flush is greater" $ do
                let lhs = toCard <$> ["2D", "3D", "4D", "5D", "6D", "10S", "KS"]
                let rhs = toCard <$> ["2D", "3D", "4D", "5D", "6D", "7D", "KS"]
                evalFlush lhs `shouldSatisfy` (< evalFlush rhs)
            it "Flush is greater" $ do
                let lhs = toCard <$> ["2D", "4D", "5D", "6D", "7D", "10S", "KS"]
                let rhs = toCard <$> ["2D", "3D", "4D", "5D", "6D", "7D", "KS"]
                evalFlush lhs `shouldSatisfy` (< evalFlush rhs)
