module Main where

import Graphics.UI.Gtk

cardImage :: IO Image
cardImage = imageNew

drawDiscardPoolStatus = hBoxNew False 0
playHistoryTextView = hBoxNew False 0

infoBox :: IO VBox
infoBox = do
   box             <- vBoxNew False 0

   card            <- cardImage
   drawDiscardPool <- drawDiscardPoolStatus
   playHistory     <- playHistoryTextView

   boxPackStart box card            PackNatural 0
   boxPackEnd   box drawDiscardPool PackNatural 0
   boxPackEnd   box playHistory     PackGrow 0
   return box

gamePlayBox :: IO VBox
gamePlayBox = vBoxNew False 0

topLevelBox :: IO HBox
topLevelBox = do
   box      <- hBoxNew False 0
   info     <- infoBox
   gamePlay <- gamePlayBox
   boxPackStart box info     PackNatural 0
   boxPackEnd   box gamePlay PackGrow    0
   return box

gameWindow :: IO Window
gameWindow = do
   window <- windowNew
   box <- topLevelBox
   containerAdd window box
   return window

main :: IO ()
main = do
   initGUI
   window <- gameWindow
   onDestroy window mainQuit
   widgetShowAll window
   mainGUI

