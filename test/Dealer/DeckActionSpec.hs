{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Dealer.DeckActionSpec where

import HaskellHoldem.Dealer.DeckActions (newDeck, shuffle, runShuffle, cardAction, shuffleM)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

import HaskellHoldem.Dealer.Deck
import System.Random.Stateful
import Test.QuickCheck (quickCheck)
import Util.CardParser ()
import Util.QuickCheckInstances ()
import Util.DeckIntegrity (checkDeck)
import HaskellHoldem.Dealer.DeckActions (cardAction_)

-- WARNING : Be careful about creating mutation generators

prop_shuffle :: (RandomGen g) => g -> Deck -> Bool
prop_shuffle gen deck = deck /= (fst $ runShuffle gen deck)

spec :: StdGen -> IO ()
spec gen = do
    putStrLn "Deck Action Tests"
    putStrLn "DeckAction QuickChecks"
    quickCheck $ prop_shuffle gen newDeck

    hspec $ do
        describe "Shuffle Pure" $ do

            it "Pure Shuffling a Deck alters the deck" $ do
                fst (runShuffle gen newDeck) `shouldNotBe` newDeck
            it "Pure Shuffling with same seed" $ do
                let (deck1, _) = runShuffle gen newDeck
                let (deck2, _) = runShuffle gen newDeck
                deck1 `shouldBe` deck2
            it "Pure Shuffling with differnet seed" $ do
                let (deck1, gen2) = runShuffle gen newDeck
                let (deck2, _) = runShuffle gen2 newDeck
                deck1 `shouldNotBe` deck2
            it "Shuffling with mutation" $ do
                iogen <- newIOGenM gen
                deck1 <- shuffle newDeck iogen
                deck2 <- shuffle newDeck iogen
                deck1 `shouldNotBe` deck2
            it "Shuffling chain with mutation" $ do
                iogen <- newIOGenM gen
                deck1 <- shuffle newDeck iogen
                deck2 <- shuffle deck1 iogen
                deck1 `shouldNotBe` deck2
        checkDeck "Shuffle Deck Integrity" $ fst $ runShuffle gen newDeck

        describe "Stateful Shuffling" $ do
            it "Shuffle once in state" $ do
                iogen <- newIOGenM gen
                deck <- cardAction iogen newDeck $ do
                    shuffleM
                deck `shouldNotBe` newDeck
            it "Shuffle multiple times" $ do
                iogen <- newIOGenM gen
                deck <- cardAction iogen newDeck $ do
                    shuffleM
                    shuffleM
                    shuffleM
                    shuffleM
                deck `shouldNotBe` newDeck
            it "Shuffle states are mutatable" $ do
                iogen <- newIOGenM gen
                deck1 <- cardAction iogen newDeck $ do
                    shuffleM
                deck2 <- cardAction iogen newDeck $ do
                    shuffleM
                deck1 `shouldNotBe` deck2
            it "Shuffle gives entire deck as leftover" $ do
                iogen <- newIOGenM gen
                (deck1, deck2) <- cardAction_ iogen newDeck $ do
                    shuffleM
                    shuffleM
                deck1 `shouldBe` deck2
