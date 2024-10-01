module HaskellHoldem.Dealer.Deck where

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

data Card = Card
    { suit :: Suit
    , value :: Value
    } deriving (Eq)

data Colour
    = Black
    | Red
    | Other
    deriving (Show, Eq)

type Deck = [Card]

instance Ord Card where
    card `compare` other = value card `compare` value other

instance Show Card where
    show card = show (suit card) ++ show (value card)

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
    | suit card `elem` [Diamonds, Hearts] = Red
    | suit card `elem` [Spades, Clubs] = Black
    | otherwise = Other
