module Messages where

import GameGUI

type Power = String
type NetString = String

data InMessage =
   DiscardMessage            Int            -- Number to discard
 | ExploreMessage            [HandCard] Int -- Cards to Explore | Number to keep
 | DevelopMessage            [HandCard]     -- Cards in hand eligible for development
 | SettleMessage             [HandCard]     -- Cards in hand eligible for settling
 | ChooseConsumePowerMessage [Power]        -- Eligible Consume Powers
 | ChooseGoodMessage         [TableauCard] Int String --   Cards in tableau with an eligible good
                                                      -- | Number of goods to discard
                                                      -- | Card name whose consume power you are using
 | ProduceMessage            [TableauCard]  -- Cards in tableau with eligible to place a good on
   deriving (Show, Read)

data OutMessage =
   FinishedDiscard            [HandCard]
 | FinishedExplore            [HandCard]
 | FinishedDevelop            (Maybe HandCard)
 | FinishedSettle             (Maybe HandCard)
 | FinishedChooseConsumePower Power
 | FinishedChooseGood         [TableauCard]
 | FinishedProduce            (Maybe TableauCard)
   deriving (Show, Read)

sendMessage :: OutMessage -> IO ()
sendMessage = print
