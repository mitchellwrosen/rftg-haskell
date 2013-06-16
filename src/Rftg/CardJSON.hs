{-# LANGUAGE OverloadedStrings #-}

module Rftg.CardJSON where

import Rftg.Card

import Control.Applicative ((<$>), (<*>), liftA, pure)

import Data.Aeson
   ( FromJSON
   , Value(..)
   , (.:), (.:?)
   , eitherDecode', parseJSON, withBool, withNumber
   )
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import Data.Text (append, unpack)
import Data.Traversable (forM)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V

getBaseCards ::  IO [Card]
getBaseCards = do
   json <- BS.readFile "./cards_base.json"
   case eitherDecode' json :: Either String [Card] of
      Left err    -> error err -- error seems fine here, since bad json = broken program
      Right cards -> return cards

getTGSCards = undefined -- TODO
getRVICards = undefined -- TODO
getBOWCards = undefined -- TODO

---------------------------------------------------------------------------

badFormat :: Show s => s -> String
badFormat s = "bad format or unknown key: " ++ show s

withBool' :: Value -> Parser Bool
withBool' = withBool "" pure

withNumber' :: Value -> Parser Int
withNumber' = withNumber "" (pure . truncate)

strToInt :: String -> Parser Int
strToInt t = case reads t :: [(Int, String)] of
                 [(n, "")] -> pure n
                 _         -> fail $ "expected int-as-string, got " ++ t

-- "world" | "development"
instance FromJSON CardKind where
   parseJSON (String "world")       = pure World
   parseJSON (String "development") = pure Development
   parseJSON _                      = fail "card_kind expects 'world' or 'development'"

-- "military" | "non_military"
instance FromJSON SettleKind where
   parseJSON (String "military")     = pure Military
   parseJSON (String "non_military") = pure NonMilitary
   parseJSON _                       = fail "settle_kind expects 'military' or 'non_military'"


-- "any" | "novelty" | "rare_elements" | "genes" | "alien_technology"
instance FromJSON GoodKind where
   parseJSON (String "any")              = pure Any
   parseJSON (String "novelty")          = pure Novelty
   parseJSON (String "rare_elements")    = pure RareElements
   parseJSON (String "genes")            = pure Genes
   parseJSON (String "alien_technology") = pure AlienTechnology
   parseJSON _                           = fail "good_kind expects 'any', 'novelty', 'rare_elements', 'genes', or 'alien_technology'"

-- {
--    "production" | "windfall": GoodKind
-- }
instance FromJSON ProductionKind where
   parseJSON (Object o) = do
      let [(key, val)] = H.toList o
      case key of
         "production" -> Production <$> parseJSON val
         "windfall"   -> Windfall   <$> parseJSON val
         _            -> fail $ unpack $ "unknown production_kind " `append` key


-- "alien" | "imperium" | "rebel" | "terraforming" | "uplift"
instance FromJSON NameKind where
   parseJSON (String "alien")        = pure Alien
   parseJSON (String "imperium")     = pure Imperium
   parseJSON (String "rebel")        = pure Rebel
   parseJSON (String "terraforming") = pure Terraforming
   parseJSON (String "uplift")       = pure Uplift
   parseJSON _                       = fail "name_kind expects 'alien', 'imperium', 'rebel', 'terraforming', or 'uplift'"

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

-- "red" | "blue"
instance FromJSON StartWorldColor where
   parseJSON (String "red")  = pure Red
   parseJSON (String "blue") = pure Blue
   parseJSON _               = fail "start_world_color expects 'red' or 'blue'"

-- constant: (int)
-- variable:
--    [
--       { (int): VPQualifier }
--       { (int): VPQualifier }
--       ...
--    ]
instance FromJSON VPValue where
   parseJSON (Number n) = pure $ Constant (truncate n)
   parseJSON (Array arr) = Variable <$>
      V.toList <$>
         forM arr (\arrItem ->
            case arrItem of
               Object o -> do
                  let [(key, val)] = H.toList o
                  amount <- strToInt (unpack key)
                  qualifier <- parseJSON val
                  pure (amount, qualifier)
               _ -> fail $ "excepted object as element of vp_value array, got " ++ show arrItem
         )
   parseJSON other = fail $ badFormat other

-- CardQualifier | "per_military" | "per_prestige" | "galactic_exchange"
instance FromJSON VPQualifier where
   parseJSON (String "per_military")          = pure PerMilitary
   parseJSON (String "per_prestige")          = pure PerPrestige
   parseJSON (String "galactic_exchange") = pure GalacticExchange
   parseJSON v = Qualifier <$> parseJSON v

-- "no_qualifier" ... "alien_technology" ... etc
-- for cost qualifiers:
--    "<=6" for "at most 6"
--    "6"   for "exactly 6"
instance FromJSON CardQualifier where
   parseJSON (String "no_qualifier") = pure NoQualifier
   parseJSON (String "this_card")    = pure ThisCardQualifier
   parseJSON (String "military")     = pure MilitaryQualifier
   parseJSON (String "non_military") = pure NonMilitaryQualifier
   parseJSON (String "production") = pure ProductionQualifier
   parseJSON (String "windfall") = pure WindfallQualifier
   parseJSON (String "any_good") = pure AnyGoodQualifier
   parseJSON (String "novelty") = pure NoveltyQualifier
   parseJSON (String "genes") = pure GenesQualifier
   parseJSON (String "rare_elements") = pure RareElementsQualifier
   parseJSON (String "alien_technology") = pure AlienTechnologyQualifier
   parseJSON (String "alien") = pure AlienQualifier
   parseJSON (String "imperium") = pure ImperiumQualifier
   parseJSON (String "rebel") = pure RebelQualifier
   parseJSON (String "terraforming") = pure TerraformingQualifier
   parseJSON (String "uplift") = pure UpliftQualifier
   parseJSON (String "chromosome") = pure ChromosomeQualifier
   parseJSON (String "prestige") = pure PrestigeQualifier
   parseJSON (String s) =
      if "<=" `isPrefixOf` s'
      then CostAtMost  <$> strToInt (drop 2 s')
      else CostExactly <$> strToInt s'
      where s' = unpack s
   parseJSON (Object o) = do
      let [(key, val)] = H.toList o
      case key of
         "not" -> Not <$> parseJSON val
         "and" ->
            case val of
               Array a ->
                  if V.length a == 2
                  then And <$> parseJSON (a `V.unsafeIndex` 0) <*> parseJSON (a `V.unsafeIndex` 1)
                  else fail failStr
               _ -> fail failStr
            where failStr = "'and' key expects 2-element array value"
   parseJSON other = fail $ badFormat other

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

-- no-argument constructor:  string
-- one-argument constructor: object with constructor as key, arg as value
-- two-argument constructor: object with constructor as key, array of args as value
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

instance FromJSON Power where
   parseJSON (Object o) =
      Power <$> o .:? "action"
            <*> o .:  "reward"
            <*> o .:  "times"

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

instance FromJSON Card where
   parseJSON (Object o) =
      Card <$> o .:  "name"
           <*> o .:  "kind"
           <*> o .:? "settle_kind"
           <*> o .:? "production_kind"
           <*> liftA (fromMaybe [])    (o .:? "name_kinds")
           <*> liftA (fromMaybe False) (o .:? "is_chromosome")
           <*> liftA (fromMaybe False) (o .:? "is_prestige")
           <*> o .:  "cost"
           <*> o .:  "vp_value"
           <*> o .:? "start_world_color"
           <*> o .:? "start_hand"
           <*> liftA (fromMaybe []) (o .:?  "explore_powers")
           <*> liftA (fromMaybe []) (o .:?  "develop_powers")
           <*> liftA (fromMaybe []) (o .:?  "settle_powers")
           <*> liftA (fromMaybe []) (o .:?  "trade_powers")
           <*> liftA (fromMaybe []) (o .:? "consume_powers")
           <*> liftA (fromMaybe []) (o .:?  "produce_powers")
           <*> o .:? "other_power"
