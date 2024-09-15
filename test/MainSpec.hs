module Main (main) where
import Test.Hspec

import DeckSpec (spec)

main :: IO ()
main = do
    hspec $ DeckSpec.spec
