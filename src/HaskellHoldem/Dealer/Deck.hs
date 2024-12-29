{-# LANGUAGE InstanceSigs #-}
module HaskellHoldem.Dealer.Deck where
import Data.Word (Word32)
import Data.Bits
import Data.Array.Unboxed


newtype Card = Card Word32 deriving (Eq, Ord)
type Deck = [Card]

type Suit = Word32
type Value = Word32

-- Card construction
-- +--------+--------+--------+--------+
-- |xxxbbbbb|bbbbbbbb|cdhsrrrr|xxpppppp|
-- +--------+--------+--------+--------+

-- p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
-- r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
-- cdhs = suit of card (bit turned on based on suit of card)
-- b = bit turned on depending on rank of card

allSuits :: [Suit]
allSuits = [(2^s) `shiftL` 12 | s <- [0..3] :: [Word32]]

allValues :: [Value]
allValues = [((!) primes (v)) + ((fromIntegral v) `shiftL` 8) + ((2 ^ v) `shiftL` 16) | v <- [0..12]::[Int]]

newDeck :: Deck
newDeck = [Card (s + v) | s <- allSuits, v <- allValues]

getValueBits :: Card -> Word32
getValueBits (Card card) = card `shiftR` 16

getSuit :: Card -> Suit
getSuit (Card card) = (card `shiftR` 12) .&. 0xF

getValue :: Card -> Value
getValue (Card card) = (card `shiftR` 8) .&. 0xF

instance Show Card where
    show :: Card -> String
    show card = showSuit (getSuit card) ++ showValue (getValue card)

showSuit:: Suit -> String
showSuit s = case s of
        1 -> "♠"
        2 -> "♥"
        4 -> "♦"
        8 -> "♣"
        _ -> "?"

-- PERF: Concat might slow down?
showValue :: Value -> String
showValue v = (!) cardValues (fromIntegral v)

primes :: UArray Int Word32
primes = listArray (0, 12) [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]

cardValues :: Array Int String
cardValues = listArray (0, 12) ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]

makeCard :: Value -> Suit -> Card
makeCard v s = Card $
    (!) primes (fromIntegral v ) +
    (v  `shiftL` 8) +
    (s  `shiftL` 12) +
    ((2 ^ s)  `shiftL` 16)

