module Dealer.DeckActionSpec where
import HaskellHoldem.Dealer.Deck
import HaskellHoldem.Dealer.DeckActions
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck, Property)
import Util.CardParser ()
import Util.QuickCheckInstances ()
import System.Random.Stateful
import Test.QuickCheck.Property ((===))

prop_shuffleChangesDeckWithGen :: (StatefulGen g m) => g -> Deck -> m Property
prop_shuffleChangesDeckWithGen gen deck = do
    shuffledDeck <- shuffle gen deck
    pure $ (deck /= shuffledDeck) === True


spec :: IOGenM StdGen ->  IO ()
spec gen = do
    putStrLn "DeckAction QuickChecks"
    shuffle_change <- prop_shuffleChangesDeckWithGen gen newDeck
    quickCheck shuffle_change

    hspec $ do
        describe "Deck LongChecks" $ do
            it "New deck has correct number of cards" $ do
                length newDeck `shouldBe` 13
