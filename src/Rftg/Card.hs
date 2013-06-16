{-# LANGUAGE TemplateHaskell #-}

module Rftg.Card where

import Control.Lens.TH (makeLenses)

data CardKind = World
              | Development deriving (Show)

data SettleKind = Military
                | NonMilitary deriving (Show)

data GoodKind = Any
              | Novelty
              | RareElements
              | Genes
              | AlienTechnology
              deriving (Eq, Show)

data ProductionKind = Production GoodKind
                    | Windfall GoodKind
                    deriving (Show)

data NameKind = Alien
              | Imperium
              | Rebel
              | Terraforming
              | Uplift
              deriving (Show)

data CostKind = AtMost Int
              | Exactly Int
              deriving (Show)

data StartWorldColor = Red | Blue deriving (Show)

data VPValue = Constant Int
             | Variable [(Int, VPQualifier)] -- Qualifier and amount expressed in a map
             deriving (Show)

-- VP qualifiers, used to score 6-devs. In the case of GalacticExchange, the
-- int paired with it (to form a VPValue) doesn't matter.
data VPQualifier = Qualifier CardQualifier -- name, production type, etc
                 | PerMilitary             -- per military
                 | PerPrestige             -- per prestige
                 | GalacticExchange        -- 1/3/6/10 for 1/2/3/4 different kinds of goods (production or windfall)
                 deriving (Show)

data CardQualifier =
     NoQualifier
   | Not CardQualifier
   | And CardQualifier CardQualifier
   | ThisCardQualifier         -- This card
   | MilitaryQualifier         -- Military world
   | NonMilitaryQualifier      -- Non-military world
   | ProductionQualifier       -- Production world
   | WindfallQualifier         -- Windfall world
   | AnyGoodQualifier          -- Any good (production or windfall)
   | NoveltyQualifier          -- Novelty good
   | GenesQualifier            -- Genes good
   | RareElementsQualifier     -- Rare elements good
   | AlienTechnologyQualifier  -- Alien technology good
   | AlienQualifier            -- "Alien" card
   | ImperiumQualifier         -- "Imperium" card
   | RebelQualifier            -- "Rebel" card
   | TerraformingQualifier     -- "Terraforming" card
   | UpliftQualifier           -- "Uplift" card
   | ChromosomeQualifier       -- Chromosome symbol
   | PrestigeQualifier         -- Prestige symbol FIXME: is this used?
   | NameQualifier String      -- The name of the card (used for scoring 6-devs)
   | CostAtMost    Int         -- Costs at most
   | CostExactly   Int         -- Costs exactly FIXME: is this used?
   deriving (Show)

-- An Action is a condition/trigger for which a Reward is accrued. Powers that
-- have no such conditions have NoAction.
data Action =
     Develop               CardQualifier   -- Develop a card of the specified kind
   | DiscardFromHand       Int             -- Discard exactly n cards from hand
   | DiscardFromTableau    CardQualifier   -- Discard the specified card from tableau
   | Pay                                   -- Play a card by paying at least 1 for it after discounts
   | Produce               CardQualifier   -- Produce on the specified card
   | Settle                CardQualifier   -- Settle the specified card
   | SpendGood             [CardQualifier] -- Spend goods of the specified kinds
   | SpendDifferent        Int             -- Spend n different kinds of goods
   | SpendDifferentUpTo    Int             -- Spend up to n different kinds of goods -- FIXME: necessary?
   | SpendAllGoods                         -- Spend all goods
   | SpendPrestige                         -- Spend 1 prestige
   | Trade                 CardQualifier   -- Trade the specified card
   deriving (Show)

data Reward =
     AnteAndDrawIfLucky                        -- From RvI
   | Draw                   Int                -- Draw n cards
   | DrawForKindsProduced                      -- Draw 1 card for each different kind of good produced
   | DrawForKindProduced    GoodKind           -- Draw 1 card for each of the specified kind of good produced
   | DrawForMostProduced    GoodKind      Int  -- Draw n cards if the player produced more of the specified good than any other player
   | DrawForTableau         CardQualifier Int  -- Draw n cards for each of the specified cards in the tableau
   | DrawIfLucky                               -- Name 1-7, flip top card, keep if correct guess
   | DrawThenDiscard        Int                -- Draw n cards, then discard 1 card
   | DrawSavedCards                            -- Draw all cards saved under this one
   | Keep                   Int                -- Keep n extra cards from Exploring
   | MixAndMatch                               -- Combine hand with draw before discard
   | PayForMilitary         CardQualifier Int  -- Pay for specified military world as non-military world for n extra cost
   | PlusMilitary           CardQualifier Int  -- Gain n (specialized) military
   | PlusMilitaryForTableau CardQualifier Int  -- Gain n military for each of the specified cards in tableau
   | PlusMilitaryIfTableau  CardQualifier Int  -- Gain n military if the specified cards in tableau
   | Prestige               Int                -- Gain n prestige
   | PrestigeForMost        CardQualifier      -- Gain 1 prestige if the player has more of the specified worlds than any other player
   | ProduceCard            CardQualifier      -- Produce a good on the specified world, if empty
   | ReceiveGood            CardQualifier      -- Put a good on top of the specified world after placing it
   | ReduceCost             CardQualifier Int  -- Reduce the specified card's cost by n
   | ReduceCostToZero       CardQualifier      -- Reduce the specified card's cost to 0
   | ReducePayForMilitary   Int                -- Reduce pay-for-military cost by n (does not give pay-for-military power)
   | SaveCard                                  -- Place a card under this one
   | SettleSecondWorld                         -- Settle a second world (without powers from first, do not draw a Settle bonus if the player chose Settle)
   | SpendForTradePrice     Bool               -- Spend good for its trade price, can-use-trade-powers
   | TakeoverDefense        CardQualifier Int  -- Gain n takeover defense for each of the specified cards in the tableau
   | TakeOverImperium                          -- Take over an Imperium military world
   | TakeOverRebel                             -- Take over a Rebel military world
   | TemporaryMilitary      Int                -- Gain n military this phase
   | VictoryPoints          Int           Bool -- Gain n VPs, can-times-two
   | VictoryPointsVariable  Int           Bool -- Gain a variable amount of VPS (dependent upon the action) for that amound plus n, can-times-two
   deriving (Show)

data Power = Power { _pAction  :: Maybe Action
                   , _pRewards :: [Reward]
                   , _pTimes   :: Int
                   } deriving (Show)

makeLenses ''Power

data OtherPower = LargerHandLimit Int
                | SeeOtherPlayersActions
                | GainOtherPlayersDiscards
                deriving (Show)

data Card =
   Card { _cName            :: String
        , _cKind            :: CardKind
        , _cSettleKind      :: Maybe SettleKind      -- Nothing for development cards
        , _cProductionKind  :: Maybe ProductionKind  -- Nothing for development cards, non-production worlds
        , _cNameKinds       :: [NameKind]            -- [] if no names
        , _cChromosome      :: Bool                  -- Chromosome symbol
        , _cPrestige        :: Bool                  -- Prestige symbol
        , _cCost            :: Int
        , _cVpValue         :: VPValue
        , _cStartWorldColor :: Maybe StartWorldColor -- Nothing if not start world
        , _cStartHand       :: Maybe Int             -- Nothing if not part of a starter hand
        , _cExplorePowers   :: [Power]
        , _cDevelopPowers   :: [Power]
        , _cSettlePowers    :: [Power]
        , _cTradePowers     :: [Power]
        , _cConsumePowers   :: [Power]
        , _cProducePowers   :: [Power]
        , _cOtherPower      :: Maybe OtherPower
        } deriving (Show)

makeLenses ''Card

