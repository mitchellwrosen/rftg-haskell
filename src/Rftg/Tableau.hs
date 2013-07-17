{-# LANGUAGE TemplateHaskell #-}

module Rftg.Tableau where

import Rftg.Card

import Control.Lens.Getter ((^.))
import Control.Lens.TH (makeLenses)

type Tableau = [TableauCard]

data TableauCard =
   TableauCard { _tCard            :: Card
               , _tGood            :: Maybe GoodKind
               , _tCardsUnderneath :: [Card]
               } deriving (Show)

makeLenses ''TableauCard

-- Returns whether or not the specified card has the specified good.
hasGood :: TableauCard -> GoodKind -> Bool
hasGood card good_kind =
   case card ^. tGood of
      Nothing         -> False
      Just good_kind' -> good_kind == Any || good_kind == good_kind'
