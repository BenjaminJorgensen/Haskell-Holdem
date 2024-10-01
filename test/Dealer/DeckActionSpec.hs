{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Dealer.DeckActionSpec where

import HaskellHoldem.Dealer.DeckActions (newDeck, shuffle, cardAction, shuffleM)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe, shouldContain, shouldNotContain)

import HaskellHoldem.Dealer.Deck ( Deck )
import System.Random.Stateful ( newIOGenM, RandomGen, StdGen )
import Test.QuickCheck (quickCheck)
import Util.CardParser ()
import Util.QuickCheckInstances ()
import Util.DeckIntegrity (checkDeck, deckSize, isSublist)
import HaskellHoldem.Dealer.DeckActions (cardAction_)
import HaskellHoldem.Dealer.DeckActions (draw)
import HaskellHoldem.Dealer.DeckActions (runShuffle, runDraw, drawM)
import Control.Monad
import Control.Monad.State

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

        describe "Pure Drawing" $ do
            it "Draw a card" $ do
                let ((card, _), _) = runDraw gen newDeck
                card `shouldBe` (head $ newDeck)
            it "Drawing multiple cards" $ do
                let ((card, deck), g) = runDraw gen newDeck
                let ((card2, _), _) = runDraw g deck
                card `shouldNotBe` card2
            it "Drawing cards removes them from the deck" $ do
                let ((card, deck), _) = runDraw gen newDeck
                [card] `shouldNotContain` deck
            it "Never run out of cards" $ do
                iogen <- newIOGenM gen
                largeDeck <- replicateM (deckSize*2) $ draw newDeck iogen
                length largeDeck `shouldBe` deckSize * 2
                isSublist largeDeck largeDeck `shouldBe` True

        describe "Stateful drawing" $ do
            it "Can draw statefully" $ do
                iogen <- newIOGenM gen
                (newhand, remainingDeck) <- cardAction_ iogen newDeck $ do
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                length newhand `shouldBe` 3
                isSublist newhand newDeck `shouldBe` True
                isSublist newhand remainingDeck `shouldBe` False
            it "Generator Retains randomness" $ do
                iogen <- newIOGenM gen
                (newhand, remainingDeck) <- cardAction_ iogen (newDeck) $ do
                    shuffleM
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                (newhand2, remainingDeck2) <- cardAction_ iogen newDeck $ do
                    shuffleM
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                newhand `shouldNotBe` newhand2
                remainingDeck `shouldNotBe` remainingDeck2
