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
               ,GameGUI()
               ) where

import Data.Maybe
import Graphics.UI.Gtk

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
