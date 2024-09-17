module Main (main) where
import Test.Hspec
import System.Random
import System.Random.Stateful (newIOGenM)

import qualified DeckSpec as D
import qualified RulesSpec as R
import qualified CardParserSpec as CP

main :: IO ()
main = do
    gen <- getStdGen
    ioGen <- newIOGenM gen
    putStrLn "Testing Card Parser"
    hspec $ CP.spec 
    putStrLn "Testing Deck"
    hspec $ D.spec ioGen
    putStrLn "Testing Poker Rules"
    hspec $ R.spec
