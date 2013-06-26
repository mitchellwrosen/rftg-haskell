module GameGUI (emptyGameGUI
               ,getMenu
               ,getCard
               ,getDrawDiscardPool
               ,getPlayHistory
               ,getExplore
               ,getDevelop
               ,getSettle
               ,getConsume
               ,getProduce
               ,getDone
               ,getContext
               ,getHand
               ,getOpponents
               ,getPlayerTableau
               ,getGUIState
               ,setMenu
               ,setCard
               ,setDrawDiscardPool
               ,setPlayHistory
               ,setExplore
               ,setDevelop
               ,setSettle
               ,setConsume
               ,setProduce
               ,setDone
               ,setContext
               ,setHand
               ,setOpponents
               ,setPlayerTableau
               ,setGUIState
               ,GameGUI()
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

myfromJust (Just a) = a
myfromJust _ = error "bloom"

emptyGameGUI = GameGUI Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing

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
    menu :: Maybe MenuBar
   ,card :: Maybe Image
   ,drawDiscardPool :: Maybe Label
   ,playHistory     :: Maybe TextView
   ,explore :: Maybe Label
   ,develop :: Maybe Label
   ,settle  :: Maybe Label
   ,consume :: Maybe Label
   ,produce :: Maybe Label
   ,done    :: Maybe Button
   ,context :: Maybe HBox
   ,hand    :: Maybe DrawingArea
   ,opponents :: Maybe HBox
   ,playerTableau :: Maybe DrawingArea
   ,guiState :: Maybe (IORef GUIState)
}

setMenu            val gui = gui { menu = Just val }
setCard            val gui = gui { card = Just val }
setDrawDiscardPool val gui = gui { drawDiscardPool = Just val }
setPlayHistory     val gui = gui { playHistory = Just val }
setExplore         val gui = gui { explore = Just val }
setDevelop         val gui = gui { develop = Just val }
setSettle          val gui = gui { settle = Just val }
setConsume         val gui = gui { consume = Just val }
setProduce         val gui = gui { produce = Just val }
setDone            val gui = gui { done = Just val }
setContext         val gui = gui { context = Just val }
setHand            val gui = gui { hand = Just val }
setOpponents       val gui = gui { opponents = Just val }
setPlayerTableau   val gui = gui { playerTableau = Just val }
setGUIState        val gui = gui { guiState = Just val }

getMenu            = fromJust . menu
getCard            = fromJust . card
getDrawDiscardPool = fromJust . drawDiscardPool
getPlayHistory     = fromJust . playHistory
getExplore         = fromJust . explore
getDevelop         = fromJust . develop
getSettle          = fromJust . settle
getConsume         = fromJust . consume
getProduce         = fromJust . produce
getDone            = fromJust . done
getContext         = fromJust . context
getHand            = fromJust . hand
getOpponents       = fromJust . opponents
getPlayerTableau   = fromJust . playerTableau
getGUIState        = fromJust . guiState
