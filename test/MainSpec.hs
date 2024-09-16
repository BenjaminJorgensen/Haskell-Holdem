module Main (main) where
import Test.Hspec
import System.Random
import Control.Monad.State
import System.Random.Stateful (runStateGen, newIOGenM)

import DeckSpec (spec)

main :: IO ()
main = do
    gen <- getStdGen
    ioGen <- newIOGenM gen
    hspec $ DeckSpec.spec ioGen
