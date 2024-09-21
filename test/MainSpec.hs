module Main (main) where
import System.Random ( getStdGen )
import System.Random.Stateful (newIOGenM)

import qualified Util.CardParserSpec as CP
import qualified Dealer.DeckSpec as D
import qualified Dealer.DeckActionSpec as D
import qualified Dealer.JudgementSpec as R

main :: IO ()
main = do
    gen <- getStdGen
    ioGen <- newIOGenM gen
    putStrLn "Testing Card Parser"
    CP.spec 
    putStrLn "Testing Deck"
    D.spec
    -- putStrLn "Testing Poker Rules"
    -- hspec $ R.spec
