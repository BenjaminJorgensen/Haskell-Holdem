{-# LANGUAGE InstanceSigs #-}
module Util.QuickCheckInstances where
import Test.QuickCheck
import HaskellHoldem.Dealer.Deck (Card, makeCard, allValues, allSuits)

instance Arbitrary Card where
    arbitrary :: Gen Card
    arbitrary = do
        v <- elements allValues
        s <- elements allSuits
        pure $ makeCard v s


