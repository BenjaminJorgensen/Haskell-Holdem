module CardParser where
import Deck
import Data.Char (digitToInt)

-- WARNING: 
-- This module will throw an error if parsing fails
-- Use this only for testing and NEVER in gameplay

genError :: [Char] -> a
genError reason = error $ "Cannot parse card: " ++ reason

toCard :: String -> Card
toCard [] = genError "no card to parse"
toCard (_:[]) = genError "incorrect number of values, no suit or no value"
toCard (v:s:[]) = Card {value=toValue v, suit=toSuit s}
toCard ('1':'0':s:[]) = Card {value=Ten, suit=toSuit s}
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
