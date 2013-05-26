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

data SymbolKind = Chromosome
                | Circle
                | Diamond

data StartWorldColor = Red | Blue

-- Powers ----------------------------------------------------------------------
-- An Action is a condition/trigger for which a Reward is accrued. Powers that
-- have no such conditions have NoAction.
data Action =
     ANoAction
   | AConsume               [CardQualifier] -- Consume the specified cards
   | AConsumeDifferent      Int             -- Consume n different kinds of goods
   | AConsumeDifferentUpTo  Int             -- Consume up to n different kinds of goods
   | ADevelop               CardQualifier   -- Develop a card of the specified kind
   | ADevelopByPayment                      -- Develop a card by paying at least 1 for it after discounts
   | ADiscardFromHandUpTo   Int             -- Discard up to n cards from hand (can be 1)
   | ADiscardFromTableau    CardQualifier   -- Discard the specified card from tableau
   | AProduce               CardQualifier   -- Produce on the specified card
   | ASettle                CardQualifier   -- Settle the specified card
   | ASpendGood             GoodKind        -- Spend a good of the specified kind
   | ATrade                 CardQualifier   -- Trade the specified card

data Reward =
     RAnteAndDrawIfLucky                      -- From RvI
   | RDraw                  Int               -- Draw n cards
   | RDrawForKindsProduced                    -- Draw 1 card for each different kind of good produced
   | RDrawForKindProduced   GoodKind          -- Draw 1 card for each of the specified kind of good produced
   | PDrawForMostProduced   GoodKind      Int -- Draw n cards if the player produced more of the specified good than any other player
   | RDrawForTableau        CardQualifier Int -- Draw n cards for each of the specified cards in the tableau
   | RDrawIfLucky                             -- Name 1-7, flip top card, keep if correct guess
   | RDrawThenDiscard       Int               -- Draw n cards, then discard 1 card
   | RKeep                  Int               -- Keep n extra cards from Exploring
   | RMilitary              CardQualifier Int -- Gain n (specialized) military
   | RMixAndMatch                             -- Combine hand with draw before discard
   | RPayForMilitary        CardQualifier Int -- Pay for specified military world as non-military world for n extra cost
   | RPlaceUnder                              -- Place a card under this one
   | RPrestige              Int               -- Gain n prestige
   | RProduce               CardQualifier     -- Produce a good on the specified world, if empty
   | RReduceCost            CardQualifier Int -- Reduce the specified card's cost by n
   | RReduceCostToZero      CardQualifier     -- Reduce the specified card's cost to 0
   | RReducePayForMilitary  Int               -- Reduce pay-for-military cost by n (does not give pay-for-military power)
   | RSettleSecondWorld                       -- Settle a second world (without powers from first, do not draw a Settle bonus if the player chose Settle)
   | RTakeoverDefense       CardQualifier Int -- Gain n takeover defense for each of the specified cards in the tableau
   | RTakeOverImperium                        -- Take over an Imperium military world
   | RTakeOverRebel                           -- Take over a Rebel military world
   | RTemporaryMilitary     Int               -- Gain n military this phase
   | RTrade                                   -- Trade good for its trade price
   | RTradeNoTradePowers                      -- FIXME: can this be simplified?
   | RTradeNoTimesTwo                         -- FIXME: can this be simplified?
   | RVictoryPoints         Int               -- Gain n VPs

data Power = Power Action Reward Int -- Perform action for reward up to n times

data CardQualifier =
     CQNoQualifier
   | CQNot                  CardQualifier
   | CQAnd                  CardQualifier CardQualifier
   | CQThisCard
   | CQSettleQualifier      SettleKind
   | CQProductionQualifier  ProductionKind
   | CQGoodQualifier        GoodKind
   | CQDevelopmentQualifier DevelopmentKind
   | CQNameQualifier        NameKind

data Card =
   Card { name            :: String
        , kind            :: CardKind
        , settleKind      :: Maybe SettleKind      -- Nothing for development cards
        , productionKind  :: Maybe ProductionKind  -- Nothing for development cards
        , nameKinds       :: [NameKind]            -- [] if no names
        , symbolKind      :: Maybe SymbolKind      -- Nothing if no symbol
        , cost            :: Int
        , vp              :: Int
        , startWorldColor :: Maybe StartWorldColor -- Nothing if not start world
        , explorePowers   :: [Power]
        , developPowers   :: [Power]
        , settlePowers    :: [Power]
        , tradePowers     :: [Power]
        , consumePowers   :: [Power]
        , producePowers   :: [Power]
        }
