{-# OPTIONS_GHC -Wno-orphans #-}

module Util.CardParserSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck
    ( Suit(Clubs, Diamonds, Spades, Hearts),
      Value(Ten, Two, King, Ace),
      makeCard )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Util.CardParser ( toCard )
-- import Util.QuickCheckInstances (CardCode (..))
-- import Test.QuickCheck.Test (quickCheck)

-- prop_card_parser :: CardCode -> Bool
-- prop_card_parser (CardCode cc) = case toCard cc of
--     (Card _) -> True
--  TODO: REnable this quickcheck

spec :: IO ()
spec = do
    putStrLn "Parser QuickChecks"
    -- quickCheck prop_card_parser
    hspec
        $ describe "Parser LongChecks"
        $ do
              it "Diamonds" $ do
                  let card = toCard "2D"
                  card `shouldBe` makeCard Two Diamonds
              it "Spades" $ do
                  let card = toCard "KS"
                  card `shouldBe` makeCard King Spades
              it "Hearts" $ do
                  let card = toCard "AH"
                  card `shouldBe` makeCard Ace Hearts
              it "Clubs" $ do
                  let card = toCard "10C"
                  card `shouldBe` makeCard Ten Clubs
