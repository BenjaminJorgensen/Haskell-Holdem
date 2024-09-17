{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where
import System.Random
import System.Random.Stateful (newIOGenM)
import Deck
import Rules
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)

-- Dealing Five hands of poker
main :: IO ()
main = do
    gen <- (newIOGenM =<< getStdGen)
    (players, community) <- evalCardState gen newDeck $ do
        shuffleM
        players <- replicateM 5 $ replicateM 2 drawM
        community <- replicateM 5 drawM
        pure (players, community)
    print $ community
    print $ players
    putStrLn "Did player one get a flush?"
    let flush = fromMaybe [0,0,0] (evalFlush $ (head players) ++ community)
    print flush
