module Main where
import Deck (newDeck, threeCoins)
import System.Random
import Control.Monad.State (runState)

main :: IO ()
main = do
    print $ runState (threeCoins (1,1,1)) (mkStdGen 69)
