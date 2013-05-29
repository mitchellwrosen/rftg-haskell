{-# LANGUAGE OverloadedStrings #-}

module Rftg.Card where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson
   ( FromJSON
   , Value(..)
   , (.:), (.:?)
   , eitherDecode', parseJSON, withBool, withNumber
   )
import Data.Aeson.Types (Parser)
import Data.Text (append, unpack)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

badFormat :: Show s => s -> String
badFormat s = "bad format or unknown key: " ++ show s

withBool' :: Value -> Parser Bool
withBool' = withBool "" pure

withNumber' :: Value -> Parser Int
withNumber' = withNumber "" (pure . truncate)

data CardKind = World
              | Development deriving (Show)

-- "world" | "development"
instance FromJSON CardKind where
   parseJSON (String "world")       = pure World
   parseJSON (String "development") = pure Development
   parseJSON _                      = fail "card_kind expects 'world' or 'development'"

data SettleKind = Military
                | NonMilitary deriving (Show)

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

-- {
--    "production" | "windfall": GoodKind
-- }
data ProductionKind = Production GoodKind
                    | Windfall GoodKind
                    deriving (Show)

instance FromJSON ProductionKind where
   parseJSON (Object o) = do
      let [(key, val)] = H.toList o
      case key of
         "production" -> Production <$> parseJSON val
         "windfall"   -> Windfall   <$> parseJSON val
         _            -> fail $ unpack $ "unknown production_kind " `append` key

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
   parseJSON _                     = fail "symbol_kind expects 'chromosome', 'circle', or 'diamond'"

data CostKind = AtMost Int
              | Exactly Int
              deriving (Show)

-- {
--    "at_most" | "exactly": (int)
-- }
instance FromJSON CostKind where
   parseJSON (Object o) =
      case H.toList o of
         [(key, Number n)] ->
            case key of
               "at_most" -> AtMost  <$> n'
               "exactly" -> Exactly <$> n'
               _         -> fail $ "unknown cost_kind " ++ unpack key
            where n' :: Parser Int
                  n' = (pure . truncate) n
         _ -> fail $ badFormat o
   parseJSON other = fail $ badFormat other

data StartWorldColor = Red | Blue deriving (Show)

-- "red" | "blue"
instance FromJSON StartWorldColor where
   parseJSON (String "red")  = pure Red
   parseJSON (String "blue") = pure Blue
   parseJSON _               = fail "start_world_color expects 'red' or 'blue'"

data VPValue = Constant Int
             | Variable deriving (Show)

-- "(int)" | "?"
instance FromJSON VPValue where
   parseJSON (String "?") = pure Variable
   parseJSON (String s) =
      case reads (unpack s) :: [(Int, String)] of
         [(n, "")] -> pure $ Constant n
         _         -> fail $ unpack $ "unknown vp_value " `append` s

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

-- no-argument constructor:  string
-- one-argument constructor: object with constructor as key, arg as value
-- two-argument constructor: object with constructor as key, array of args as value
instance FromJSON CardQualifier where
   parseJSON (String "no_qualifier") = pure NoQualifier
   parseJSON (String "this_card")    = pure ThisCard
   parseJSON (Object o) =
      case H.toList o of
         [(key, val@(Object _))] ->
            case key of
               "not"                  -> Not                 <$> parseJSON val
               "settle_qualifier"     -> SettleQualifier     <$> parseJSON val
               "production_qualifier" -> ProductionQualifier <$> parseJSON val
               "good_qualifier"       -> GoodQualifier       <$> parseJSON val
               "name_qualifier"       -> NameQualifier       <$> parseJSON val
               "symbol_qualifier"     -> SymbolQualifier     <$> parseJSON val
               "cost_qualifier"       -> CostQualifier       <$> parseJSON val
               _                      -> fail $ badFormat o
         [(key, Array a)] ->
            if V.length a == 2
            then do
               let arg0 = a `V.unsafeIndex` 0
               let arg1 = a `V.unsafeIndex` 1

               case key of
                  "and" -> And <$> parseJSON arg0 <*> parseJSON arg1
                  _     -> fail $ badFormat o
            else fail $ badFormat o
         _ -> fail $ badFormat o
   parseJSON other = fail $ badFormat other

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

-- no-argument constructor:  string
-- one-argument constructor: object with constructor as key, arg as value
instance FromJSON Action where
   parseJSON (String "pay")             = pure Pay
   parseJSON (String "spend_all_goods") = pure SpendAllGoods
   parseJSON (String "spend_prestige")  = pure SpendPrestige
   parseJSON (Object o) =
      case H.toList o of
         [(key, Number n)] ->
            case key of
               "discard_from_hand"     -> DiscardFromHand    <$> n'
               "spend_different"       -> SpendDifferent     <$> n'
               "spend_different_up_to" -> SpendDifferentUpTo <$> n'
               _                       -> fail $ badFormat o
            where n' :: Parser Int
                  n' = (pure . truncate) n
         [(key, val@(Object _))] ->
            case key of
               "develop"              -> Develop            <$> parseJSON val
               "discard_from_tableau" -> DiscardFromTableau <$> parseJSON val
               "produce"              -> Produce            <$> parseJSON val
               "settle"               -> Settle             <$> parseJSON val
               "trade"                -> Trade              <$> parseJSON val
               _                      -> fail $ badFormat o
         [(key, Array a)] ->
            case key of
               "spend_good" -> SpendGood <$> (fmap V.toList . V.mapM parseJSON) a
               _            -> fail $ badFormat o
         _ -> fail $ badFormat o
   parseJSON other = fail $ badFormat other

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
   parseJSON (String "ante_and_draw_if_lucky") = pure AnteAndDrawIfLucky
   parseJSON (String "draw_if_lucky")          = pure DrawIfLucky
   parseJSON (String "draw_saved_cards")       = pure DrawSavedCards
   parseJSON (String "mix_and_match")          = pure MixAndMatch
   parseJSON (String "save_card")              = pure SaveCard
   parseJSON (String "settle_second_world")    = pure SettleSecondWorld
   parseJSON (String "take_over_imperium")     = pure TakeOverImperium
   parseJSON (String "take_over_rebel")        = pure TakeOverRebel
   parseJSON (Object o) =
      case H.toList o of
         [(key, Number n)] ->
            case key of
               "draw"                    -> Draw                 <$> n'
               "draw_then_discard"       -> DrawThenDiscard      <$> n'
               "keep"                    -> Keep                 <$> n'
               "prestige"                -> Prestige             <$> n'
               "reduce_pay_for_military" -> ReducePayForMilitary <$> n'
               "temporary_military"      -> TemporaryMilitary    <$> n'
               _                         -> fail $ badFormat o
            where n' :: Parser Int
                  n' = (pure . truncate) n
         [(key, Bool b)] ->
            case key of
               "spend_for_trade_price" -> pure $ SpendForTradePrice b
               _                       -> fail $ badFormat o
         [(key, val@(Object _))] ->
            case key of
               "draw_for_kind_produced" -> DrawForKindProduced <$> parseJSON val
               "prestige_for_most"      -> PrestigeForMost     <$> parseJSON val
               "produce_card"           -> ProduceCard         <$> parseJSON val
               "receive_good"           -> ReceiveGood         <$> parseJSON val
               "reduce_cost_to_zero"    -> ReduceCostToZero    <$> parseJSON val
               _                        -> fail $ badFormat o
         [(key, Array a)] ->
            if V.length a == 2
            then do
               let arg0 = a `V.unsafeIndex` 0
               let arg1 = a `V.unsafeIndex` 1

               case key of
                  "draw_for_most_produced"  -> DrawForMostProduced   <$> parseJSON arg0   <*> withNumber' arg1
                  "draw_for_tableau"        -> DrawForTableau        <$> parseJSON arg0   <*> withNumber' arg1
                  "pay_for_military"        -> PayForMilitary        <$> parseJSON arg0   <*> withNumber' arg1
                  "plus_military"           -> PlusMilitary          <$> parseJSON arg0   <*> withNumber' arg1
                  "reduce_cost"             -> ReduceCost            <$> parseJSON arg0   <*> withNumber' arg1
                  "victory_points"          -> VictoryPoints         <$> withNumber' arg0 <*> withBool' arg1
                  "victory_points_variable" -> VictoryPointsVariable <$> withNumber' arg0 <*> withBool' arg1
                  _                         -> fail $ badFormat o
            else fail $ badFormat o
         _ -> fail $ badFormat o
   parseJSON other = fail $ badFormat other

data Power = Power { action  :: Maybe Action
                   , rewards :: [Reward]
                   , times   :: Int
                   } deriving (Show)

instance FromJSON Power where
   parseJSON (Object o) =
      Power <$> o .:? "action"
            <*> o .:  "reward"
            <*> o .:  "times"

data OtherPower = LargerHandLimit Int
                | SeeOtherPlayersActions
                | GainOtherPlayersDiscards
                deriving (Show)

instance FromJSON OtherPower where
   parseJSON (String "see_other_players_actions")   = pure SeeOtherPlayersActions
   parseJSON (String "gain_other_players_discards") = pure GainOtherPlayersDiscards
   parseJSON (Object o) =
      case H.toList o of
         [(key, Number n)] ->
            case key of
               "larger_hand_limit" -> LargerHandLimit <$> (pure . truncate) n
               _                   -> fail $ badFormat o
         _ -> fail $ badFormat o
   parseJSON other = fail $ badFormat other

data Card =
   Card { name            :: String
        , kind            :: CardKind
        , settleKind      :: Maybe SettleKind      -- Nothing for development cards
        , productionKind  :: Maybe ProductionKind  -- Nothing for development cards, non-production worlds
        , nameKinds       :: [NameKind]            -- [] if no names
        , symbolKind      :: Maybe SymbolKind      -- Nothing if no symbol
        , cost            :: Int
        , vpValue         :: VPValue
        , startWorldColor :: Maybe StartWorldColor -- Nothing if not start world
        , explorePowers   :: [Power]
        , developPowers   :: [Power]
        , settlePowers    :: [Power]
        , tradePowers     :: [Power]
        , consumePowers   :: [Power]
        , producePowers   :: [Power]
        , otherPower      :: Maybe OtherPower
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
           <*> o .:  "vp_value"
           <*> o .:? "start_world_color"
           <*> o .:  "explore_powers"
           <*> o .:  "develop_powers"
           <*> o .:  "settle_powers"
           <*> o .:  "trade_powers"
           <*> o .:  "consume_powers"
           <*> o .:  "produce_powers"
           <*> o .:? "other_powers"

getBaseCards ::  IO (Either String [Card])
getBaseCards = do
   json <- BS.readFile "./cards_base.json"
   return (eitherDecode' json :: Either String [Card])
