module HaskellHoldem.Dealer.Deck (Card(..), Suit(..), Value(..), Deck, getSuit, getValue, makeCard) where
import Data.Word (Word32)
import Data.Array.Unboxed
import Data.Bits
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (intercalate)

data Suit
    = Diamonds
    | Hearts
    | Spades
    | Clubs
    deriving (Eq, Ord, Bounded, Enum, Read)

data Value
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Bounded, Ord, Enum, Read)

-- +--------+--------+--------+--------+
-- |xxxbbbbb|bbbbbbbb|cshdrrrr|xxpppppp|
-- +--------+--------+--------+--------+

-- p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
-- r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
-- cdhs = suit of card (bit turned on based on suit of card)
-- b = bit turned on depending on rank of card

newtype Card = Card Word32 deriving (Eq, Ord)

data Colour
    = Black
    | Red
    | Other
    deriving (Show, Eq)

type Deck = [Card]

instance Show Card where
    show card = show (getSuit card) ++ show (getValue card)

instance Show Suit where
    show Diamonds = "♦"
    show Hearts = "♥"
    show Spades = "♠"
    show Clubs = "♣"

instance Show Value where
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"
    show v = show $ fromEnum v + 2

-- Gets the colour of a card
getColour :: Card -> Colour
getColour card
    | getSuit card `elem` [Diamonds, Hearts] = Red
    | getSuit card `elem` [Spades, Clubs] = Black
    | otherwise = Other

-- Card construction
-- +--------+--------+--------+--------+
-- |xxxbbbbb|bbbbbbbb|cdhsrrrr|xxpppppp|
-- +--------+--------+--------+--------+

-- p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
-- r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
-- cdhs = suit of card (bit turned on based on suit of card)
-- b = bit turned on depending on rank of card

primes :: UArray Int Word32
primes = listArray (0, 12) [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]

value2prime :: Value -> Word32
value2prime v = (!) primes (fromEnum v)

value2int :: Value -> Word32
value2int v =  (fromIntegral (fromEnum v)) `shiftL` 8

suit2bit :: Suit -> Word32
suit2bit s = (2^(fromEnum s)) `shiftL` 12

value2bit :: Value -> Word32
value2bit v = (2^(fromEnum v)) `shiftL` 16

makeCard :: Value -> Suit -> Card
makeCard v s = Card $ foldl1 (+) [value2prime v, value2int v, value2bit v, suit2bit s]

-- Getting Values
getSuit :: Card -> Suit
getSuit (Card card) = case (.&.) (card `shiftR` 12) 15 of
    1 -> Diamonds
    2 -> Hearts
    4 -> Spades
    _ -> Clubs

getValue :: Card -> Value
getValue (Card card) = toEnum ((fromIntegral card `shiftR` 8) .&. 15)


-- Function to split a string into groups of n characters, grouping from the right
groupNFromRight :: Int -> String -> [String]
groupNFromRight n str = reverse $ map reverse $ groupN n (reverse str)
  where
    groupN _ [] = []
    groupN ns s = take ns s : groupN ns (drop ns s)

showbin :: Card -> String
showbin (Card card) = intercalate " " (groupNFromRight 8 (showIntAtBase 2 intToDigit card ""))
