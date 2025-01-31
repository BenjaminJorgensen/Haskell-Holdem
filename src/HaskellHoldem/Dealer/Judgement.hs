module HaskellHoldem.Dealer.Judgement where
import HaskellHoldem.Dealer.Deck
import Util.FlushTable
import HaskellHoldem.Dealer.Hand (Hand (..), constructHand)
import Data.Word (Word32)

-- +--------+--------+--------+--------+
-- |xxxbbbbb|bbbbbbbb|cdhsrrrr|xxpppppp|
-- +--------+--------+--------+--------+

-- p = prime number of rank (deuce=2,trey=3,four=5,...,ace=41)
-- r = rank of card (deuce=0,trey=1,four=2,five=3,...,ace=12)
-- cdhs = suit of card (bit turned on based on suit of card)
-- b = bit turned on depending on rank of card



evalFlush :: [Card] -> Int
evalFlush hand = flushLookup $ (getFlushKey.constructHand) hand

getFlushKey :: Hand -> Int
getFlushKey (Hand _ bitss) = case filter (\x -> fst x >= 5) bitss of
    [] -> -1
    [x] -> (fromIntegral . snd) x
    _ -> -1

fastHash :: Word32 -> Int
fastHash = undefined
