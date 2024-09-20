
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.CardParserSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck (Card(..), Suit(..), Value(..))
import Test.Hspec ( hspec, describe, it, shouldBe )
import Util.CardParser ( toCard )
import Util.QuickCheckInstances (CardCode (..))
import Test.QuickCheck.Test (quickCheck)

prop_card_parser :: CardCode -> Bool
prop_card_parser (CardCode cc) = case toCard cc of
    Card _ _ -> True

spec :: IO ()
spec = do
    putStrLn "Parser QuickChecks"
    quickCheck prop_card_parser
    hspec
        $ describe "Parser LongChecks"
        $ do
              it "Diamonds" $ do
                  let card = toCard "2D"
                  card `shouldBe` Card {value = Two, suit = Diamonds}
              it "Spades" $ do
                  let card = toCard "KS"
                  card `shouldBe` Card {value = King, suit = Spades}
              it "Hearts" $ do
                  let card = toCard "AH"
                  card `shouldBe` Card {value = Ace, suit = Hearts}
              it "Clubs" $ do
                  let card = toCard "10C"
                  card `shouldBe` Card {value = Ten, suit = Clubs}
