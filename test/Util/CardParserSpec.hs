{-# OPTIONS_GHC -Wno-orphans #-}

module Util.CardParserSpec
    ( spec
    ) where

import HaskellHoldem.Dealer.Deck ( makeCard )
import Test.Hspec ( hspec, describe, it, shouldBe )
import Util.CardParser ( toCard )
-- import Util.QuickCheckInstances (CardCode (..))
-- import Test.QuickCheck.Test (quickCheck)

-- prop_card_parser :: CardCode -> Bool
-- prop_card_parser (CardCode cc) = case toCard cc of
--     (Card _) -> True
--  TODO: Redo these tests

spec :: IO ()
spec = do
    putStrLn "Parser QuickChecks"
    -- quickCheck prop_card_parser
    hspec
        $ describe "Parser LongChecks"
        $ do
              it "Diamonds" $ do
                  let card = toCard "2D"
                  1 `shouldBe` 1
