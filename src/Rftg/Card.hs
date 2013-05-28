{-# LANGUAGE OverloadedStrings #-}

module Rftg.Card where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson
   ( FromJSON
   , Value(..)
   , (.:), (.:?)
   , parseJSON, withBool, withNumber
   )
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Number (Number(..))
import Data.Text (Text, unpack)

import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

-- Cards -----------------------------------------------------------------------
data CardKind = World | Development deriving (Show)

-- "world" | "development"
instance FromJSON CardKind where
   parseJSON (String "world")       = pure World
   parseJSON (String "development") = pure Development
   parseJSON _                      = fail "card_kind expects 'world' or 'development'"

data SettleKind = Military | NonMilitary deriving (Show)

-- "military" | "non_military"
instance FromJSON SettleKind where
   parseJSON (String "military")     = pure Military
   parseJSON (String "non_military") = pure NonMilitary
   parseJSON _                       = fail "settle_kind expects 'military' or 'non_military'"

data GoodKind = Any
              | Novelty
              | RareElements
              | Genes
              | AlienTechnology
              deriving (Show)

-- "any" | "novelty" | "rare_elements" | "alien_technology"
instance FromJSON GoodKind where
   parseJSON (String "any")              = pure Any
   parseJSON (String "novelty")          = pure Novelty
   parseJSON (String "rare_elements")    = pure RareElements
   parseJSON (String "alien_technology") = pure AlienTechnology
   parseJSON _                           = fail "good_kind expects 'any', 'novelty', 'rare_elements', 'genes', or 'alien_technology'"

-- { "production" | "windfall" : GoodKind }
data ProductionKind = Production GoodKind
                    | Windfall GoodKind
                    deriving (Show)

instance FromJSON ProductionKind where
   parseJSON (Object o) = do
      let [(key, Array arr)] = H.toList o
      case key of
         "production" ->
            if V.length arr == 1
            then Production <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "production expects good_kind argument"
         "windfall" ->
            if V.length arr == 1
            then Windfall <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "windfall expects one good_kind argument"
         _ -> fail "production_kind object expects key 'production' or 'windfall'"

data NameKind = Alien
              | Imperium
              | Rebel
              | Terraforming
              | Uplift
              deriving (Show)

-- "alien" | "imperium" | "rebel" | "terraforming" | "uplift"
instance FromJSON NameKind where
   parseJSON (String "alien")        = pure Alien
   parseJSON (String "imperium")     = pure Imperium
   parseJSON (String "rebel")        = pure Rebel
   parseJSON (String "terraforming") = pure Terraforming
   parseJSON (String "uplift")       = pure Uplift
   parseJSON _                       = fail "name_kind expects 'alien', 'imperium', 'rebel', 'terraforming', or 'uplift'"

data SymbolKind = Chromosome
                | Circle
                | Diamond
                deriving (Show)

-- "chromosome" | "circle" | "diamond"
instance FromJSON SymbolKind where
   parseJSON (String "chromosome") = pure Chromosome
   parseJSON (String "circle")     = pure Circle
   parseJSON (String "diamond")    = pure Diamond
   parseJSON _                      = fail "symbol_kind expects 'chromosome', 'circle', or 'diamond'"

data CostKind = AtMost Int
              | Exactly Int
              deriving (Show)

data StartWorldColor = Red | Blue deriving (Show)

data VPValue = Constant Int
             | Variable deriving (Show)

-- Powers ----------------------------------------------------------------------
data CardQualifier =
     NoQualifier
   | Not                  CardQualifier
   | And                  CardQualifier CardQualifier
   | ThisCard
   | SettleQualifier      SettleKind
   | ProductionQualifier  ProductionKind
   | GoodQualifier        GoodKind
   | NameQualifier        NameKind
   | SymbolQualifier      SymbolKind
   | CostQualifier        CostKind
   deriving (Show)

instance FromJSON CardQualifier where
   parseJSON (Object o) = do
      let [(key, Array arr)] = H.toList o
      case key of
         "no_qualifier" ->
            if V.length arr == 0
            then pure NoQualifier
            else fail "no_qualifier expects no arguments"
         "not" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "not expects card_qualifier argument"
         "and" ->
            if V.length arr == 2
            then And <$> parseJSON (arr `V.unsafeIndex` 0)
                     <*> parseJSON (arr `V.unsafeIndex` 1)
            else fail "and expects two card_qualifier arguments"
         "this_card" ->
            if V.length arr == 0
            then pure ThisCard
            else fail "this_card expects no arguments"
         "settle_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "settle_qualifier expects settle_kind argument"
         "production_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "production_qualifier expects production_kind argument"
         "good_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "good_qualifier expects good_kind argument"
         "name_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "name_qualifier expects name_kind argument"
         "symbol_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "symbol_qualifier expects symbol_kind argument"
         "cost_qualifier" ->
            if V.length arr == 1
            then Not <$> parseJSON (arr `V.unsafeIndex` 0)
            else fail "cost_qualifier expects cost_kind argument"
         _ -> fail $ "unknown card_qualifier " ++ unpack key

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

instance FromJSON Reward where
   parseJSON (Object o) = do
      let [(key, Array arr)] = H.toList o

      let numArgs = V.length arr
      let arg0 = arr `V.unsafeIndex` 0
      let arg1 = arr `V.unsafeIndex` 1

      case key of
         "draw" ->
            if numArgs == 1
            then Draw <$> withNumber' key (pure . numberToInt) arg0
            else fail $ badNumArgs key numArgs 1
         "draw_if_lucky" ->
            if numArgs == 0
            then pure DrawIfLucky
            else fail $ badNumArgs key numArgs 0
         "draw_for_kind_produced" ->
            if numArgs == 1
            then DrawForKindProduced <$> parseJSON arg0
            else fail $ badNumArgs key numArgs 1
         "draw_for_kinds_produced" ->
            if numArgs == 0
            then pure DrawForKindsProduced
            else fail $ badNumArgs key numArgs 0
         "draw_for_most_produced" ->
            if numArgs == 2
            then DrawForMostProduced <$> parseJSON arg0
                                     <*> withNumber' key (pure . numberToInt) arg1
            else fail $ badNumArgs key numArgs 2
         "draw_for_tableau" ->
            if numArgs == 2
            then DrawForTableau <$> parseJSON arg0
                                <*> withNumber' key (pure . numberToInt) arg1
            else fail $ badNumArgs key numArgs 2
         "keep" ->
            if numArgs == 1
            then Keep <$> withNumber' key (pure . numberToInt) arg0
            else fail $ badNumArgs key numArgs 1
         "pay_for_military" ->
            if numArgs == 2
            then PayForMilitary <$> parseJSON arg0
                                <*> withNumber' key (pure . numberToInt) arg1
            else fail $ badNumArgs key numArgs 2
         "plus_military" ->
            if numArgs == 2
            then PlusMilitary <$> parseJSON arg0
                              <*> withNumber' key (pure . numberToInt) arg1
            else fail $ badNumArgs key numArgs 2
         "produce_card" ->
            if numArgs == 1
            then ProduceCard <$> parseJSON arg0
            else fail $ badNumArgs key numArgs 1
         "reduce_cost" ->
            if numArgs == 2
            then ReduceCost <$> parseJSON arg0
                            <*> withNumber' key (pure . numberToInt) arg1
            else fail $ badNumArgs key numArgs 2
         "reduce_cost_to_zero" ->
            if numArgs == 1
            then ReduceCostToZero <$> parseJSON arg0
            else fail $ badNumArgs key numArgs 1
         "spend_for_trade_price" ->
            if numArgs == 1
            then SpendForTradePrice <$> withBool' key pure arg0
            else fail $ badNumArgs key numArgs 1
         "temporary_military" ->
            if numArgs == 1
            then TemporaryMilitary <$> withNumber' key (pure . numberToInt) arg0
            else fail $ badNumArgs key numArgs 1
         "victory_points" ->
            if numArgs == 2
            then VictoryPoints <$> withNumber' key (pure . numberToInt) arg0
                               <*> withBool' key pure arg1
            else fail $ badNumArgs key numArgs 2
         "victory_points_variable" ->
            if numArgs == 2
            then VictoryPointsVariable <$> withNumber' key (pure . numberToInt) arg0
                                       <*> withBool' key pure arg1
            else fail $ badNumArgs key numArgs 2
         _ -> fail $ "unknown reward " ++ unpack key

      where badNumArgs :: Text -> Int -> Int -> String
            badNumArgs key actual expected =
               unpack key ++ " received " ++ show actual ++ " args (expected "
               ++ show expected ++ ")"

            withBool' :: Text -> (Bool -> Parser a) -> Value -> Parser a
            withBool' = withBool . (\key -> unpack key ++ " expected bool argument")

            withNumber' :: Text -> (Number -> Parser a) -> Value -> Parser a
            withNumber' = withNumber . (\key -> unpack key ++ " expected number argument")

numberToInt :: Number -> Int
numberToInt (I n) = fromIntegral n

data Power = Power { action  :: Maybe Action
                   , rewards :: [Reward]
                   , times   :: Int
                   } deriving (Show)

data OtherPower = LargerHandLimit Int
                | SeeOtherPlayersActions
                | GainOtherPlayersDiscards
                deriving (Show)


data Card =
   Card { name            :: String
        , kind            :: CardKind
        , settleKind      :: Maybe SettleKind      -- Nothing for development cards
        , productionKind  :: Maybe ProductionKind  -- Nothing for development cards, non-production worlds
        , nameKinds       :: [NameKind]            -- [] if no names
        , symbolKind      :: Maybe SymbolKind      -- Nothing if no symbol
        , cost            :: Int
        , vp              :: VPValue
        , startWorldColor :: Maybe StartWorldColor -- Nothing if not start world
        , explorePowers   :: [Power]
        , developPowers   :: [Power]
        , settlePowers    :: [Power]
        , tradePowers     :: [Power]
        , consumePowers   :: [Power]
        , producePowers   :: [Power]
        } deriving (Show)

instance FromJSON Card where
   parseJSON (Object o) =
      Card <$> o .:  "name"
           <*> o .:  "kind"
           <*> o .:? "settle_kind"
           <*> o .:? "production_kind"
           <*> o .:  "name_kinds"
           <*> o .:? "symbol_kind"
           <*> o .:  "cost"
           <*> o .:  "vp"
           <*> o .:? "start_world_color"
           <*> o .:  "explore_powers"
           <*> o .:  "develop_powers"
           <*> o .:  "settle_powers"
           <*> o .:  "trade_powers"
           <*> o .:  "consume_powers"
           <*> o .:  "produce_powers"
