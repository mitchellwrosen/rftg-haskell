module GameGUI
    (
      GameGUI(..)
    , GamePlayGUI(..)
    , InfoGUI(..)
    , Selected(..)
    , HasGood
    , TableauCard(..)
    , HandCard(..)
    , Hand
    , Tableau
    , Activity(..)
    , GUIState(..)
    ) where

import Graphics.UI.Gtk
import Data.IORef (IORef)

type Name = String
type HasGood = Bool

data Selected = Disabled | Selected | UnSelected
    deriving (Eq, Show, Read)
data TableauCard = TableauCard Name Selected HasGood
    deriving (Show, Read)
data HandCard = HandCard Name Selected
    deriving (Show, Read)

instance Eq TableauCard where
    -- Equality check on Name and HasGood, ignore Selected
    (TableauCard a1 _ a2) == (TableauCard b1 _ b2) = a1 == b1 && a2 == b2

instance Eq HandCard where
    -- Equality check on Name, ignore Selected
    (HandCard a _) == (HandCard b _) = a == b

type Hand = [HandCard]
type Tableau = [TableauCard]

data Activity = Discard
              | Explore
              | Develop
              | Settle
              | ChooseConsumePower
              | ChooseGood
              | Produce

data InfoGUI = InfoGUI
    { getCard            :: Image
    , getDrawDiscardPool :: Label
    , getPlayHistory     :: TextView
    }

data GamePlayGUI = GamePlayGUI
    { getOpponents     :: HBox
    , getPlayerTableau :: DrawingArea
    , getExplore       :: Label
    , getDevelop       :: Label
    , getSettle        :: Label
    , getConsume       :: Label
    , getProduce       :: Label
    , getDone          :: Button
    , getContext       :: HBox
    , getHand          :: DrawingArea
    }

data GameGUI = GameGUI
    { getMenu        :: MenuBar
    , getInfoGUI     :: InfoGUI
    , getGamePlayGUI :: GamePlayGUI
    , getGUIState    :: IORef GUIState
    }

data GUIState = GUIState
    { currentHand     :: Hand
    , exploreCards    :: Hand
    , currentTableau  :: Tableau
    , currentActivity :: Activity
    , numDiscard      :: Int
    , consumePowers   :: ComboBox
    }
