module GameGUI (
                GameGUI(..)
               ,Selected(..)
               ,HasGood(..)
               ,TableauCard(..)
               ,HandCard(..)
               ,Hand(..)
               ,Tableau(..)
               ,Activity(..)
               ,GUIState(..)
               ) where

import Data.Maybe
import Graphics.UI.Gtk
import Data.IORef

type Name = String
data Selected = Disabled | Selected | UnSelected
   deriving (Eq, Show, Read)
type HasGood = Bool
data TableauCard = TableauCard Name Selected HasGood
   deriving (Show, Read)
data HandCard = HandCard Name Selected
   deriving (Show, Read)

instance Eq TableauCard where
   (TableauCard a1 _ a2) == (TableauCard b1 _ b2) = a1 == b1 && a2 == b2

instance Eq HandCard where
   (HandCard a _) == (HandCard b _) = a == b

type Hand = [HandCard]
type Tableau = [TableauCard]

data Activity = Discard | Explore | Develop | Settle | ChooseConsumePower | ChooseGood | Produce
data GUIState = GUIState {
    currentHand :: Hand
   ,exploreCards :: Hand
   ,currentTableau :: Tableau
   ,currentActivity :: Activity
   ,numDiscard :: Int
   ,consumePowers :: Maybe ComboBox
}

data GameGUI = GameGUI {
    getMenu ::  MenuBar
   ,getCard ::  Image
   ,getDrawDiscardPool ::  Label
   ,getPlayHistory     ::  TextView
   ,getExplore ::  Label
   ,getDevelop ::  Label
   ,getSettle  ::  Label
   ,getConsume ::  Label
   ,getProduce ::  Label
   ,getDone    ::  Button
   ,getContext ::  HBox
   ,getHand    ::  DrawingArea
   ,getOpponents ::  HBox
   ,getPlayerTableau ::  DrawingArea
   ,getGUIState ::  (IORef GUIState)
}
