module Main (main) where
import System.Random ( getStdGen )
import System.Random.Stateful (newIOGenM)

import qualified Util.CardParserSpec as CP
import qualified Dealer.DeckSpec as D
import qualified Dealer.DeckActionSpec as DA
import qualified Dealer.JudgementSpec as R

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn "Testing Card Parser"
    CP.spec 
    putStrLn "Testing Deck"
    D.spec
    putStrLn "Testing Deck Actions"
    DA.spec gen
    -- putStrLn "Testing Poker Rules"
    -- hspec $ R.spec
