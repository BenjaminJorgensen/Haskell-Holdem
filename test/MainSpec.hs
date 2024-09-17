module Main (main) where
import Test.Hspec
import System.Random
import System.Random.Stateful (newIOGenM)

import qualified DeckSpec as D
import qualified RulesSpec as R

main :: IO ()
main = do
    gen <- getStdGen
    ioGen <- newIOGenM gen
    print "Testing Deck"
    hspec $ D.spec ioGen
    print "Testing Poker Rules"
    hspec $ R.spec
