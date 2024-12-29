module Util.CardParser where

import Data.Char (digitToInt)
import HaskellHoldem.Dealer.Deck (Card, Suit, Value, makeCard)

-- WARNING 
-- This module will throw an error if parsing fails
-- Use this only for testing and NEVER in gameplay
genError :: [Char] -> a
genError reason = error $ "Cannot parse card: " ++ reason

toCard :: String -> Card
toCard [] = genError "no card to parse"
toCard [_] = genError "incorrect number of values, no suit or no value"
toCard [v, s] = makeCard (toValue v) (toSuit s)
toCard ['1', '0', s] = makeCard 8 (toSuit s)
toCard (_:_:e) = genError $ "incorrect number of values, to many values supplied: extra values are " ++ e

toValue :: Char -> Value
toValue 'A' = 12
toValue 'K' = 11
toValue 'Q' = 10
toValue 'J' = 9
toValue c 
    | let num = fromIntegral $ digitToInt c
        in num <= 9 && num >= 2 = fromIntegral $ digitToInt c - 2
    | otherwise = genError $ "Could not parse card value, got " ++ [c]

toSuit :: Char -> Suit
toSuit 'S' = 1
toSuit 'H' = 2
toSuit 'D' = 4
toSuit 'C' = 8
toSuit err = genError $ "Couldn't parse suit, got " ++ [err]
