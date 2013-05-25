module Rftg.Card where

-- Cards -----------------------------------------------------------------------
data CardKind = World | Development

data SettleKind = NonMilitary | Military

data ProductionKind = NoProduction
                    | Production GoodKind
                    | Windfall GoodKind

data GoodKind = AnyGood
              | NoveltyGoods
              | RareElements
              | Genes
              | AlienTechnology

data DevelopmentKind = OtherDevelopment
                     | SixCostDevelopment

data NameKind = Alien
              | Imperium
              | Rebel
              | Terraforming
              | Uplift

data StartWorldColor = Red | Blue

-- Powers ----------------------------------------------------------------------
-- Explore --
data ExplorePower =
     EPDrawExtra Int -- Draw n extra cards
   | EPKeepExtra Int -- Keep n extra cards
   | EPMixAndMatch   -- Combine hand with draw before discard
   | EPAction ExploreAction ExploreReward

data ExploreAction =
     EADiscard Int -- Discard n cards from hand

data ExploreReward =
     ERPrestige Int -- Gain n prestige

-- Develop --
data DevelopPower =
     DPDraw Int            -- Draw n cards at the start of the development phase
   | DPDrawThenDiscard Int -- Draw n cards, then discard 1 card
   | DPReduceCost Int      -- Reduce development cost by n
   | DPAction DevelopAction DevelopReward

data DevelopAction =
     DADevelop CardQualifier -- Develop a card of the specified kind
   | DASpendGood GoodKind    -- Spend a good
   | DADevelopByPayment      -- Develop a card by paying at least 1 for it after discounts

data DevelopReward =
     DRDraw Int       -- Draw n cards
   | DRPlaceUnder     -- Place a card under this one
   | DRPrestige Int   -- Gain n prestige
   | DRReduceCost Int -- Reduce cost by n

-- Settle --
data SettlePower =
     SPMilitary CardQualifier Int       -- Gain n (specialized) military
   | SPPayForMilitary CardQualifier Int -- Pay for specified military world as a non-military world for n extra cost
   | SPReduceCost CardQualifier Int     -- Reduce settle cost by n
   | SPSettleSecondWorld                -- Settle a second world (without powers from first, do not draw a Settle bonus if the player chose Settle)
   | SPAction SettleAction SettleReward

data SettleAction =
     SADiscardFromHandUpTo Int          -- Discard up to n cards from hand
   | SADiscardFromTableau CardQualifier -- Discard the specified card from tableau
   | SASettle CardQualifier             -- Settle the specified card

data SettleReward =
     SRDraw Int                       -- Draw n cards
   | SRTemporaryMilitary Int          -- Gain n military this phase
   | SRReduceCost CardQualifier Int   -- Reduce the specified card's cost by n
   | SRReduceCostToZero CardQualifier -- Reduce the specified card's cost to 0

-- Trade --
data TradePower =
     TPAction TradeAction TradeReward

data TradeAction =
     TATrade CardQualifier -- Trade the specified card

data TradeReward =
     TADraw Int -- Draw n (extra) cards

data ConsumePower =
     CPDraw Int                                        -- Draw n cards
   | CPDrawIfLucky                                     -- Name 1-7, flip top card, keep if correct guess
   | CPAction ConsumeAction ConsumeReward Int          -- Perform this consume action up to n times

data ConsumeAction =
     CAConsume [CardQualifier] -- Consume the specified cards
   | CAConsumeThreeDifferent   -- Consume three different kinds of goods
   | CADiscardFromHand

data ConsumeReward =
     CRVictoryPoints Int                 -- Take n VPs
   | CRCards Int                         -- Draw n cards
   | CRTradePrice ConsumeRewardQualifier -- Trade good for its trade price, per any qualifiers

data ConsumeRewardQualifier = CPQNoQualifier
                            | CPQNoTradePowers
                            | CPQNoTimesTwo

-- Produce --
data ProducePower =
     PDraw Int                   -- Draw n cards
   | PDrawForKind GoodKind       -- Draw 1 card for each of the specified kind of good produced
   | PDrawFor CardQualifier      -- Draw 1 card for each of the specified cards in the tableau
   | PDrawForMost GoodKind Int   -- Draw n cards if the player produced more of the specified good than any other player
   | PDrawForDifferentKinds      -- Draw 1 card for each different kind of good produced
   | PProduceGood                -- Produce a good (if this world has no good)
   | PProduceWindfall GoodKind   -- Produce on a windfall of specified good
   | PPAction ProduceAction ProduceReward

data ProduceAction =
     PAProduce CardQualifier -- Produce on the specified card

data ProduceReward =
     PRDraw Int -- Draw n cards

data CardQualifier =
   NoQualifier
 | Not CardQualifier
 | AllOf [CardQualifier]
 | AnyOf [CardQualifier]
 | ThisCard
 | SettleQualifier SettleKind
 | ProductionQualifier ProductionKind
 | GoodQualifier GoodKind
 | DevelopmentQualifier DevelopmentKind
 | NameQualifier NameKind

data Card =
   Card { name            :: String
        , kind            :: CardKind
        , settleKind      :: Maybe SettleKind     -- Nothing for development cards
        , productionKind  :: Maybe ProductionKind -- Nothing for development cards
        , nameKinds       :: [NameKind]
        , cost            :: Int
        , vp              :: Int
        , startWorldColor :: Maybe StartWorldColor -- Nothing if not start world
        , explorePowers :: [ExplorePower]
        , developPowers :: [DevelopPower]
        , settlePowers  :: [SettlePower]
        , tradePowers   :: [TradePower]
        , consumePowers :: [ConsumePower]
        , producePowers :: [ProducePower]
        }
