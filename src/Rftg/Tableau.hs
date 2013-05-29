module Rftg.Tableau where

import Rftg.Card

type Tableau = [TableauCard]

data TableauCard =
   TableauCard { tCard            :: Card
               , tGood            :: Maybe GoodKind
               , tCardsUnderneath :: [Card]
               }
