module Util.CardParser where

import Data.Char (digitToInt)
import HaskellHoldem.Dealer.Deck (Card, Suit(..), Value(Ace, Jack, King, Queen, Ten), makeCard)

-- WARNING 
-- This module will throw an error if parsing fails
-- Use this only for testing and NEVER in gameplay
genError :: [Char] -> a
genError reason = error $ "Cannot parse card: " ++ reason

toCard :: String -> Card
toCard [] = genError "no card to parse"
toCard [_] = genError "incorrect number of values, no suit or no value"
toCard [v, s] = makeCard (toValue v) (toSuit s)
toCard ['1', '0', s] = makeCard Ten (toSuit s)
toCard (_:_:e) = genError $ "incorrect number of values, to many values supplied: extra values are " ++ e

toValue :: Char -> Value
toValue 'A' = Ace
toValue 'K' = King
toValue 'Q' = Queen
toValue 'J' = Jack
toValue c = toEnum $ digitToInt c - 2

toSuit :: Char -> Suit
toSuit 'D' = Diamonds
toSuit 'S' = Spades
toSuit 'H' = Hearts
toSuit 'C' = Clubs
toSuit err = genError $ "Couldn't parse suit, got " ++ [err]
