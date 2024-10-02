{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Util.QuickCheckInstances where
import Test.QuickCheck
import HaskellHoldem.Dealer.Deck (Card, Value, Suit, makeCard)

value_codes :: [String]
value_codes = fmap show ([2 .. 10] :: [Int]) ++ ["A", "K", "Q", "J"]

suit_codes :: [String]
suit_codes = ["D", "S", "H", "C"]

newtype CardCode = CardCode {getCode :: String} deriving Show

instance Arbitrary CardCode where
    arbitrary :: Gen CardCode
    arbitrary = do
        v <- elements value_codes
        s <- elements suit_codes
        pure $ CardCode (v ++ s)

instance Arbitrary Card where
    arbitrary :: Gen Card
    arbitrary = do
        v <- arbitraryBoundedEnum :: Gen Value
        s <- arbitraryBoundedEnum :: Gen Suit
        pure $ makeCard v s


