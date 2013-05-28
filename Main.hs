module Main where

import Debug.Trace

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.Events

drawDiscardPoolText = "Draw: 100 Discard: 0 Pool: 24"

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
   box  <- hBoxNew False 0
   opp1 <- tableauBox colorRed
   boxPackStart box opp1 PackGrow 0
   return box

phaseIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/no_phase_icon.jpg" 32 32
scoreIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/score_icon.jpg" 32 32
handIconImage     = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/hand_icon.jpg" 32 32
militaryIconImage = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/military_icon.jpg" 32 32

playerStatusBox :: IO HBox
playerStatusBox = do
   box          <- hBoxNew False 0
   empty1       <- hBoxNew False 0
   empty2       <- hBoxNew False 0
   phaseIcon    <- phaseIconImage
   scoreIcon    <- scoreIconImage
   handIcon     <- handIconImage
   militaryIcon <- militaryIconImage

   boxPackStart box empty1       PackGrow    0
   boxPackStart box phaseIcon    PackNatural 0
   boxPackStart box handIcon     PackNatural 0
   boxPackStart box scoreIcon    PackNatural 0
   boxPackStart box militaryIcon PackNatural 0
   boxPackStart box empty2       PackGrow    0
   return box

tableausBox :: IO VBox
tableausBox = do
   box       <- vBoxNew False 0
   opponents <- opponentsBox
   player    <- tableauBox colorBlue
   sep       <- hSeparatorNew
   boxPackStart box opponents PackGrow    0
   boxPackStart box sep       PackNatural 0
   boxPackStart box player    PackGrow    0
   return box

tableauBox :: Color -> IO VBox
tableauBox color = do
   box          <- vBoxNew False 0
   tableau      <- tableauDrawingArea color
   playerStatus <- playerStatusBox
   boxPackStart box tableau        PackGrow    0
   boxPackStart box playerStatus   PackNatural 0
   return box

tableauDrawingArea :: Color -> IO DrawingArea
tableauDrawingArea color = do
   drawingArea <- drawingAreaNew
   widgetModifyBg drawingArea StateNormal color
   onExpose drawingArea (drawCurrentTableau drawingArea)
   return drawingArea

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

cardImageDims = (372, 520)

currentCardHeight :: Int -> Int
currentCardHeight width =
   let (cardWidth, cardHeight) = cardImageDims
       ratio = (fromIntegral cardHeight) / (fromIntegral cardWidth)
       height = fromIntegral . round $ (fromIntegral width / 6.0) * ratio
   in height

currentHandHeight :: Int -> Int
currentHandHeight width =
   let height = currentCardHeight width
       padding = height `quot` 10
   in height + padding

drawCurrentHand :: DrawingArea -> Event -> IO Bool
drawCurrentHand drawingArea _ = do
   (width, _) <- widgetGetSize drawingArea
   drawWindow      <- widgetGetDrawWindow drawingArea
   gc              <- gcNew drawWindow
   pix             <- pixbufNewFromFileAtSize "./images/card_back.jpg" (width `quot` 6) (-1)
   drawPixbuf drawWindow gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
   return True

drawCurrentTableau :: DrawingArea -> Event -> IO Bool
drawCurrentTableau drawingArea _ = do
   drawWindow      <- widgetGetDrawWindow drawingArea
   (width, height) <- widgetGetSize drawingArea
   gc              <- gcNew drawWindow
   pix             <- pixbufNewFromFileAtSize "./images/card_back.jpg" (width `quot` 6) (-1)
   drawPixbuf drawWindow gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
   return True

requestHandDrawingAreaSize drawingArea _ = do
   (width, height) <- widgetGetSize drawingArea
   widgetSetSizeRequest drawingArea (-1) (currentHandHeight width)

-- The Drawing Area for the player's current hand.
handDrawingArea ::  IO DrawingArea
handDrawingArea = do
   drawingArea <- drawingAreaNew
   onSizeAllocate drawingArea (requestHandDrawingAreaSize drawingArea)
   onExpose drawingArea (drawCurrentHand drawingArea)
   return drawingArea

colorBlue = Color 0x9000 0xa700 0xca00
colorRed  = Color 0xe700 0x6d00 0x6400

playerBox = do
   box     <- vBoxNew False 0
   phase   <- phaseBox
   action  <- actionBox
   hand    <- handDrawingArea
   sep1    <- hSeparatorNew
   sep2    <- hSeparatorNew
   boxPackStart box phase   PackNatural 0
   boxPackStart box sep1    PackNatural 0
   boxPackStart box action  PackNatural 0
   boxPackStart box sep2    PackNatural 0
   boxPackStart box hand    PackNatural 0
   return box

gamePlayBox :: IO VBox
gamePlayBox = do
   box       <- vBoxNew False 0
   tableaus  <- tableausBox
   player    <- playerBox
   sep       <- hSeparatorNew
   boxPackStart box tableaus PackGrow    0
   boxPackStart box player   PackNatural 0
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
