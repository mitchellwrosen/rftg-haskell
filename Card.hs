module Rftg.Card where

-- Cards -----------------------------------------------------------------------
data CardKind = World | Development

data SettleKind = NonMilitary | Military

data ProductionKind = NoProduction
                    | Production GoodKind
                    | Windfall GoodKind

data GoodKind = NoveltyGoods
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
data ExplorePower =
   -- Base
     EDrawExtra Int            -- Draw n extra cards
   | EKeepExtra Int            -- Keep n extra cards
   -- RvI
   | EMixAndMatch        -- Combine hand with draw before discard
   -- BoW
   | EDiscardForPrestige -- Before drawing, may discard 1 card to gain 1 prestige

data DevelopPower =
     -- Base
     DDraw Int       -- Draw n cards at the start of the development phase
   | DReduceCost Int -- Reduce development cost by n
   | DDrawAfter Int  -- Draw n cards after developing

   -- BOW
   | DDrawThenDiscard Int               -- Draw n cards, then discard 1 card
   | DPrestigeAfter CardQualifier       -- Gain 1 prestige after developing
   | DReduceAfterSpendGood GoodKind Int -- Reduce cost by n after spending a good
   | DSaveCardFromPayment               -- Put 1 card from a development payment (after discounts) under this world

data SettlePower =
     -- Base
     SReduceCost CardQualifier Int                        -- Reduce settle cost by n
   | SDiscardReduceCostToZero CardQualifier CardQualifier -- Discard specified card (first qualifier) to reduce settle cost of specified card (second qualifier) to 0
   | SMilitary CardQualifier Int                          -- Gain n (specialized) military
   | SDiscardTemporaryMilitary CardQualifier Int          -- Discard specified card from tableau to gain n military during this phase
   | SPayForMilitary CardQualifier Int                    -- Pay for specified military world as a non-military world for n extra cost
   | SDrawAfter Int                                       -- Draw n cards after settling

data TradePower =
     -- Base
     TTrade CardQualifier Int -- Trade the specified card for n extra cards

data ConsumePower =
     -- Base
     CConsume [CardQualifier] ConsumeReward Int       -- Consume specified goods for reward, up to n times
   | CConsumeThreeDifferent ConsumeReward             -- Consume 3 different kinds of goods for reward
   | CDraw Int                                        -- Draw n cards
   | CDrawIfLucky                                     -- Name 1-7, flip top card, keep if correct guess
   | CDiscard ConsumeReward ConsumePowerQualifier Int -- Discard up to n cards from hand for reward

data ConsumeReward = CRVictoryPoints Int                -- Take n VPs
                   | CRCards Int                        -- Draw n cards
                   | CRTradePrice ConsumePowerQualifier -- Trade good for its trade price, per any qualifiers

data ConsumePowerQualifier = CPNoQualifier
                           | CPNoTradePowers
                           | CPNoTimesTwo

data ProducePower = Foo -- TODO

data CardQualifier = NoQualifier
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
