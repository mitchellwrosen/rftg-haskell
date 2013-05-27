module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.Events

drawDiscardPoolText = "Draw: 86 Discard: 0 Pool: 48"

cardImage :: IO Image
cardImage = imageNewFromFile "images/card_back.jpg"

drawDiscardPoolLabel :: IO Label
drawDiscardPoolLabel = labelNew (Just drawDiscardPoolText)

playHistoryTextView :: IO TextView
playHistoryTextView = do
   view <- textViewNew
   textViewSetEditable view False
   textViewSetCursorVisible view False
   return view

infoBox :: IO VBox
infoBox = do
   box             <- vBoxNew False 0
   card            <- cardImage
   drawDiscardPool <- drawDiscardPoolLabel
   playHistory     <- playHistoryTextView

   boxPackStart box card            PackNatural 0
   boxPackStart box drawDiscardPool PackNatural 0
   boxPackStart box playHistory     PackGrow 0
   return box

opponentsBox :: IO HBox
opponentsBox = do
   box <- hBoxNew False 0
   return box

playerStatusBox :: IO HBox
playerStatusBox = do
   view <- hBoxNew False 0
   return view
tableauBox :: IO VBox
tableauBox = do
   box          <- vBoxNew False 0
   tableau      <- tableauDrawingArea
   playerStatus <- playerStatusBox
   boxPackStart box tableau        PackGrow    0
   boxPackStart box playerStatus   PackNatural 0
   return box

strikeThroughLabel :: Label -> IO Label
strikeThroughLabel label = do
   text <- labelGetText label
   labelSetMarkup label ("<s>" ++ text ++ "</s>")
   return label

exploreLabel = labelNew (Just "Explore") >>= strikeThroughLabel
developLabel = labelNew (Just "Develop") >>= strikeThroughLabel
settleLabel  = labelNew (Just "Settle") >>= strikeThroughLabel
consumeLabel = labelNew (Just "Consume") >>= strikeThroughLabel
produceLabel = labelNew (Just "Produce") >>= strikeThroughLabel

phaseBox :: IO HBox
phaseBox = do
   box <- hBoxNew False 0
   explore <- exploreLabel
   develop <- developLabel
   settle  <- settleLabel
   consume <- consumeLabel
   produce <- produceLabel
   boxPackStart box explore PackGrow 0
   boxPackStart box develop PackGrow 0
   boxPackStart box settle  PackGrow 0
   boxPackStart box consume PackGrow 0
   boxPackStart box produce PackGrow 0
   return box

doneButton = buttonNewWithLabel "Done"

contextLabel = labelNew (Just "Choose 2 cards to discard")

contextBox = do
   box <- hBoxNew False 0
   context <- contextLabel
   boxPackStart box context PackGrow 0
   return box

actionBox :: IO HBox
actionBox = do
   box <- hBoxNew False 0
   done <- doneButton
   context <- contextBox
   boxPackStart box context PackGrow    0
   boxPackStart box done    PackNatural 0
   return box

drawCurrentHand :: DrawingArea -> Event -> IO Bool
drawCurrentHand drawingArea _ = do
   drawWindow      <- widgetGetDrawWindow drawingArea
   (width, height) <- widgetGetSize drawingArea
   gc              <- gcNew drawWindow
   pix             <- pixbufNewFromFileAtSize "./images/card_back.jpg" (width `quot` 6) height
   drawPixbuf drawWindow gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
   return True

drawCurrentTableau :: DrawingArea -> Event -> IO Bool
drawCurrentTableau drawingArea _ = do
   drawWindow      <- widgetGetDrawWindow drawingArea
   (width, height) <- widgetGetSize drawingArea
   gc              <- gcNew drawWindow
   pix             <- pixbufNewFromFileAtSize "./images/card_back.jpg" (width `quot` 6) height
   drawPixbuf drawWindow gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
   return True

-- The Drawing Area for the player's current hand.
handDrawingArea ::  IO DrawingArea
handDrawingArea = do
   drawingArea <- drawingAreaNew
   onExpose drawingArea (drawCurrentHand drawingArea)
   widgetQueueDraw drawingArea
   return drawingArea

tableauDrawingArea :: IO DrawingArea
tableauDrawingArea = do
   drawingArea <- drawingAreaNew
   onExpose drawingArea (drawCurrentTableau drawingArea)
   widgetQueueDraw drawingArea
   return drawingArea

playerBox = do
   box     <- vBoxNew False 0
   tableau <- tableauBox
   phase   <- phaseBox
   action  <- actionBox
   hand    <- handDrawingArea
   sep1    <- hSeparatorNew
   sep2    <- hSeparatorNew
   sep3    <- hSeparatorNew
   boxPackStart box tableau PackGrow    0
   boxPackStart box sep1    PackNatural 0
   boxPackStart box phase   PackNatural 0
   boxPackStart box sep2    PackNatural 0
   boxPackStart box action  PackNatural 0
   boxPackStart box sep3    PackNatural 0
   boxPackStart box hand    PackNatural 0
   return box

gamePlayBox :: IO VBox
gamePlayBox = do
   box       <- vBoxNew False 0
   opponents <- opponentsBox
   player    <- playerBox
   boxPackStart box opponents PackNatural 0
   boxPackStart box player    PackGrow    0
   return box

menuBar :: IO MenuBar
menuBar = do
   menu <- menuBarNew
   game <- menuItemNewWithLabel "Game"
   containerAdd menu game
   return menu

mainBox :: IO HBox
mainBox = do
   box      <- hBoxNew False 0
   info     <- infoBox
   gamePlay <- gamePlayBox
   sep      <- vSeparatorNew
   boxPackStart box info     PackNatural 0
   boxPackStart box sep      PackNatural 0
   boxPackStart box gamePlay PackGrow    0
   return box

topLevelBox :: IO VBox
topLevelBox = do
   box  <- vBoxNew False 0
   main <- mainBox
   menu <- menuBar
   boxPackStart box menu PackNatural 0
   boxPackStart box main PackGrow    0
   return box

gameWindow :: IO Window
gameWindow = do
   window <- windowNew
   box    <- topLevelBox
   containerAdd window box
   return window

main :: IO ()
main = do
   initGUI
   window <- gameWindow
   onDestroy window mainQuit
   widgetShowAll window
   mainGUI
