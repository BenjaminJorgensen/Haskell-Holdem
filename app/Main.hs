module Main where
import Deck (newDeck, shuffle)
import System.Random
import System.Random.Stateful (newIOGenM)

main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    print newDeck
    deck <- shuffle newDeck gen
    print deck
