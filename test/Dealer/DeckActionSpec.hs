{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Dealer.DeckActionSpec where

import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe, shouldNotContain)
import System.Random.Stateful ( newIOGenM, RandomGen, StdGen )
import Test.QuickCheck (quickCheck)
import Control.Monad ( replicateM )
import Control.Monad.State ()

import HaskellHoldem.Dealer.Deck ( Deck )
import HaskellHoldem.Dealer.DeckActions
    ( newDeck,
      shuffle,
      draw,
      shuffleM,
      drawM,
      cardAction,
      cardActionM_,
      cardActionM )

import Util.CardParser ()
import Util.QuickCheckInstances ()
import Util.DeckIntegrity (checkDeck, deckSize, isSublist)

-- WARNING : Be careful about creating mutation generators

prop_shuffle :: (RandomGen g) => g -> Deck -> Bool
prop_shuffle gen deck = deck /= (fst $ cardAction shuffle gen deck)

spec :: StdGen -> IO ()
spec gen = do
    putStrLn "Deck Action Tests"
    putStrLn "DeckAction QuickChecks"
    quickCheck $ prop_shuffle gen newDeck

    hspec $ do
        describe "Shuffle Pure" $ do

            it "Pure Shuffling a Deck alters the deck" $ do
                fst (cardAction shuffle gen newDeck) `shouldNotBe` newDeck
            it "Pure Shuffling with same seed" $ do
                let (deck1, _) = cardAction shuffle gen newDeck
                let (deck2, _) = cardAction shuffle gen newDeck
                deck1 `shouldBe` deck2
            it "Pure Shuffling with differnet seed" $ do
                let (deck1, gen2) = cardAction shuffle gen newDeck
                let (deck2, _) = cardAction shuffle gen2 newDeck
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
        checkDeck "Shuffle Deck Integrity" $ fst $ cardAction shuffle gen newDeck

        describe "Stateful Shuffling" $ do
            it "Shuffle once in state" $ do
                iogen <- newIOGenM gen
                deck <- cardActionM_ iogen newDeck $ do
                    shuffleM
                deck `shouldNotBe` newDeck
            it "Shuffle multiple times" $ do
                iogen <- newIOGenM gen
                deck <- cardActionM_ iogen newDeck $ do
                    shuffleM
                    shuffleM
                    shuffleM
                    shuffleM
                deck `shouldNotBe` newDeck
            it "Shuffle states are mutatable" $ do
                iogen <- newIOGenM gen
                deck1 <- cardActionM_ iogen newDeck $ do
                    shuffleM
                deck2 <- cardActionM_ iogen newDeck $ do
                    shuffleM
                deck1 `shouldNotBe` deck2
            it "Shuffle gives entire deck as leftover" $ do
                iogen <- newIOGenM gen
                (deck1, deck2) <- cardActionM iogen newDeck $ do
                    shuffleM
                    shuffleM
                deck1 `shouldBe` deck2

        describe "Pure Drawing" $ do
            it "Draw a card" $ do
                let ((card, _), _) = cardAction draw gen newDeck
                card `shouldBe` (head $ newDeck)
            it "Drawing multiple cards" $ do
                let ((card, deck), g) = cardAction draw gen newDeck
                let ((card2, _), _) = cardAction draw g deck
                card `shouldNotBe` card2
            it "Drawing cards removes them from the deck" $ do
                let ((card, deck), _) = cardAction draw gen newDeck
                [card] `shouldNotContain` deck
            it "Never run out of cards" $ do
                iogen <- newIOGenM gen
                largeDeck <- replicateM (deckSize*2) $ draw newDeck iogen
                length largeDeck `shouldBe` deckSize * 2
                isSublist largeDeck largeDeck `shouldBe` True

        describe "Stateful drawing" $ do
            it "Can draw statefully" $ do
                iogen <- newIOGenM gen
                (newhand, remainingDeck) <- cardActionM iogen newDeck $ do
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                length newhand `shouldBe` 3
                isSublist newhand newDeck `shouldBe` True
                isSublist newhand remainingDeck `shouldBe` False
            it "Generator Retains randomness" $ do
                iogen <- newIOGenM gen
                (newhand, remainingDeck) <- cardActionM iogen (newDeck) $ do
                    shuffleM
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                (newhand2, remainingDeck2) <- cardActionM iogen newDeck $ do
                    shuffleM
                    card1 <- drawM
                    card2 <- drawM
                    card3 <- drawM
                    pure [card1, card2, card3]
                newhand `shouldNotBe` newhand2
                remainingDeck `shouldNotBe` remainingDeck2
            it "Stateful drawing never runs out of cards" $ do
                iogen <- newIOGenM gen
                cards <- cardActionM_ iogen newDeck $ do
                    cards <- replicateM (deckSize * 2) drawM
                    pure cards
                length cards `shouldBe` (deckSize * 2)

