module Main where

import Debug.Trace

import Text.Printf
import Data.IORef
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Drawable

import System.FilePath
import System.Directory (getDirectoryContents)
import Control.Monad
import Control.Monad.State
import GameGUI

type Deck = M.Map String Pixbuf

type Name = String
type Selected = Bool
type HasGood = Bool
data TableauCard = TableauCard Name Selected HasGood
data HandCard = HandCard Name Selected

type Hand = [HandCard]
type Tableau = [TableauCard]


type StateIO a = StateT GameGUI IO a

drawDiscardPoolText :: Int -> Int -> Int -> String
drawDiscardPoolText = printf "Draw: %d Discard: %d Pool: %d"

cardImageDims = (372, 520)
cardImage :: StateIO Image
cardImage = do
   image <- liftIO $ imageNewFromFile "images/cards/card_back.jpg"
   modify (setCard image)
   return image

currentCardHeight :: Int -> Int
currentCardHeight width =
   let (cardWidth, cardHeight) = cardImageDims
       ratio = (fromIntegral cardHeight) / (fromIntegral cardWidth)
       height = fromIntegral . round $ (fromIntegral width / 6.0) * ratio
   in height

drawDiscardPoolLabel :: StateIO Label
drawDiscardPoolLabel = do
   label <- liftIO $ labelNew (Just $ drawDiscardPoolText 100 0 48)
   modify (setDrawDiscardPool label)
   return label

playHistoryTextView :: StateIO TextView
playHistoryTextView = do
   view <- liftIO textViewNew
   liftIO $ textViewSetEditable view False
   liftIO $ textViewSetCursorVisible view False
   modify (setPlayHistory view)
   return view

infoBox :: StateIO VBox
infoBox = do
   box             <- liftIO $ vBoxNew False 0
   card            <- cardImage
   drawDiscardPool <- drawDiscardPoolLabel
   playHistory     <- playHistoryTextView

   liftIO $ boxPackStart box card            PackNatural 0
   liftIO $ boxPackStart box drawDiscardPool PackNatural 0
   liftIO $ boxPackStart box playHistory     PackGrow 0
   return box

opponentsBox :: StateIO HBox
opponentsBox = do
   box  <- liftIO $ hBoxNew False 0
   opp1 <- tableauBox colorRed
   modify (setOpponents box)
   liftIO $ boxPackStart box opp1 PackGrow 0
   return box

phaseIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/no_phase_icon.png" 32 32
scoreIconImage    = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/score_icon.png" 32 32
handIconImage     = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/hand_icon.png" 32 32
militaryIconImage = imageNewFromPixbuf =<< pixbufNewFromFileAtSize "./images/military_icon.png" 32 32

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

tableausBox :: StateIO VBox
tableausBox = do
   box       <- liftIO $ vBoxNew False 0
   opponents <- opponentsBox
   player    <- tableauBox colorBlue
   sep       <- liftIO hSeparatorNew
   liftIO $ boxPackStart box opponents PackGrow    0
   liftIO $ boxPackStart box sep       PackNatural 0
   liftIO $ boxPackStart box player    PackGrow    0
   return box

tableauBox :: Color -> StateIO VBox
tableauBox color = do
   box          <- liftIO $ vBoxNew False 0
   tableau      <- liftIO $ tableauDrawingArea color
   playerStatus <- liftIO playerStatusBox
   modify (setPlayerTableau tableau)
   liftIO $ boxPackStart box tableau        PackGrow    0
   liftIO $ boxPackStart box playerStatus   PackNatural 0
   return box

tableauDrawingArea :: Color -> IO DrawingArea
tableauDrawingArea color = do
   drawingArea <- drawingAreaNew
   widgetModifyBg drawingArea StateNormal color
   return drawingArea

strikeThroughLabel :: Label -> IO Label
strikeThroughLabel label = do
   text <- labelGetText label
   labelSetMarkup label ("<s>" ++ text ++ "</s>")
   return label

exploreLabel = labelNew (Just "Explore") >>= strikeThroughLabel
developLabel = labelNew (Just "Develop") >>= strikeThroughLabel
settleLabel  = labelNew (Just "Settle")  >>= strikeThroughLabel
consumeLabel = labelNew (Just "Consume") >>= strikeThroughLabel
produceLabel = labelNew (Just "Produce") >>= strikeThroughLabel

phaseBox :: StateIO HBox
phaseBox = do
   box     <- liftIO $ hBoxNew False 0
   explore <- liftIO exploreLabel
   develop <- liftIO developLabel
   settle  <- liftIO settleLabel
   consume <- liftIO consumeLabel
   produce <- liftIO produceLabel
   modify $ (setExplore explore) .
            (setDevelop develop) .
            (setSettle  settle)  .
            (setConsume consume) .
            (setProduce produce)
   liftIO $ boxPackStart box explore PackGrow 0
   liftIO $ boxPackStart box develop PackGrow 0
   liftIO $ boxPackStart box settle  PackGrow 0
   liftIO $ boxPackStart box consume PackGrow 0
   liftIO $ boxPackStart box produce PackGrow 0
   return box

doneButton = buttonNewWithLabel "Done"

contextLabel = labelNew (Just "Choose 2 cards to discard")

contextBox = do
   box <- hBoxNew False 0
   context <- contextLabel
   boxPackStart box context PackGrow 0
   return box

actionBox :: StateIO HBox
actionBox = do
   box  <- liftIO $ hBoxNew False 0
   done <- liftIO doneButton
   context <- liftIO contextBox
   modify $ (setDone done) . (setContext context)
   liftIO $ boxPackStart box context PackGrow    0
   liftIO $ boxPackStart box done    PackNatural 0
   return box

cardPadding :: Int -> Int
cardPadding height = height `quot` 10

currentHandHeight :: Int -> Int
currentHandHeight width =
   let height = currentCardHeight width
       padding = cardPadding height
   in height + padding

lookupCardPixbuf :: Deck -> String -> Pixbuf
lookupCardPixbuf m card = fromJust $ M.lookup card m

cardBack :: Deck -> Pixbuf
cardBack deck = lookupCardPixbuf deck "card_back.jpg"

setPixbuf :: Image -> Deck -> String -> IO ()
setPixbuf image deck card = imageSetFromPixbuf image (lookupCardPixbuf deck card)

getPixbufsForHand :: Deck -> Hand -> [Pixbuf]
getPixbufsForHand deck = map (\(HandCard name _) -> lookupCardPixbuf deck name)

getPixbufsForTableau :: Deck -> Tableau -> [Pixbuf]
getPixbufsForTableau deck = map (\(TableauCard name _ _) -> lookupCardPixbuf deck name)

motionInTableau :: GameGUI -> Deck -> Tableau -> EventM EMotion Bool
motionInTableau gui deck cards = do
   let drawingArea = getPlayerTableau gui
   (width, height) <- liftIO $ widgetGetSize drawingArea
   (x, y) <- eventCoordinates
   let ndx = (round x) `quot` (width `quot` 6)
   -- TODO : check the y value too
   liftIO $ when (ndx < length cards) $
      let TableauCard name _ _ = cards !! ndx
      in setPixbuf (getCard gui) deck name
   return True

toggleCardAtIndex :: Hand -> Int -> Hand
toggleCardAtIndex [] _        = undefined
toggleCardAtIndex ((HandCard name selected):xs) 0 = HandCard name (not selected) : xs
toggleCardAtIndex (x:xs) ndx  = x : toggleCardAtIndex xs (ndx - 1)

buttonPressedInHand :: GameGUI -> Deck -> IORef Hand -> EventM EButton Bool
buttonPressedInHand gui deck cardsRef = do
   let drawingArea = getHand gui
   cards <- liftIO $ readIORef cardsRef
   (width, height) <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   (x, y) <- eventCoordinates
   let ndx = (round x) `quot` (xOffsetInHand width cards)
   liftIO $ when (ndx < length cards &&
                  (round y) > cardPadding (currentCardHeight width) &&
                  clickType == SingleClick &&
                  button == LeftButton) $ do
      writeIORef cardsRef (toggleCardAtIndex cards ndx)
      widgetQueueDraw drawingArea
   return True

motionInHand :: GameGUI -> Deck -> IORef Hand -> EventM EMotion Bool
motionInHand gui deck handRef = do
   let drawingArea = getHand gui
   cards <- liftIO $ readIORef handRef
   (width, height) <- liftIO $ widgetGetSize drawingArea
   (x, y) <- eventCoordinates
   let ndx = (round x) `quot` (xOffsetInHand width cards)
   -- TODO : check the y value too
   liftIO $ when (ndx < length cards && (round y) > cardPadding (currentCardHeight width)) $
      let HandCard name selected = cards !! ndx
      in setPixbuf (getCard gui) deck name
   return True

xOffsetInHand :: Int -> Hand -> Int
xOffsetInHand width cards = min (width `quot` 6) (width `quot` length cards)

drawCurrentHand :: DrawingArea -> Deck -> IORef Hand -> EventM EExpose Bool
drawCurrentHand drawingArea deck handRef = do
   hand            <- liftIO $ readIORef handRef
   (width, _)      <- liftIO $ widgetGetSize drawingArea
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   gc              <- liftIO $ gcNew drawWindow
   let height  = currentCardHeight width
       cards   = getPixbufsForHand deck hand
       xOffset = xOffsetInHand width hand
   pixbufs <- liftIO $ forM cards (\card -> pixbufScaleSimple card (width `quot` 6) height InterpBilinear)
   -- TODO: I don't really like these next two lines
   liftIO $ forM_ [0..length cards - 1] (\i ->
      let HandCard name selected = hand !! i
          destY = if selected
                  then 0
                  else (cardPadding height)
      in drawPixbuf drawWindow gc
                    (pixbufs !! i)                     -- pixbuf to draw
                    0 0                                -- srcx srcy
                    (xOffset * i) destY                -- destx desty
                    (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

drawCurrentTableau :: DrawingArea -> Deck -> Tableau -> EventM EExpose Bool
drawCurrentTableau drawingArea deck tableau = do
   drawWindow <- liftIO $ widgetGetDrawWindow drawingArea
   (width, _) <- liftIO $ widgetGetSize drawingArea
   gc         <- liftIO $ gcNew drawWindow
   let height  = currentCardHeight width
       cards   = getPixbufsForTableau deck tableau
       xOffset = width `quot` 6
   pixbufs <- liftIO $ forM cards (\card ->
         pixbufScaleSimple card (width `quot` 6) height InterpBilinear)

   cardBackPixbuf <- liftIO $
         pixbufScaleSimple (cardBack deck) (width `quot` 8) (height * 3 `quot` 4) InterpBilinear

   liftIO $ forM_ [0..length cards - 1] (\i -> do
      let TableauCard _ _ hasGood = tableau !! i
      drawPixbuf drawWindow gc
                 (pixbufs !! i)                     -- pixbuf to draw
                 0 0                                -- srcx srcy
                 (xOffset * i) 0                    -- destx desty
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when hasGood $ drawPixbuf drawWindow gc cardBackPixbuf 0 0
                       (xOffset * i + width `quot` 24) (height `quot` 4)
                       (-1) (-1) RgbDitherNone 0 0)
   liftIO $ widgetQueueDraw drawingArea
   return True

requestHandDrawingAreaSize drawingArea _ = do
   (width, height) <- widgetGetSize drawingArea
   widgetSetSizeRequest drawingArea (-1) (currentHandHeight width)

-- The Drawing Area for the player's current hand.
handDrawingArea ::  IO DrawingArea
handDrawingArea = do
   drawingArea <- drawingAreaNew
   onSizeAllocate drawingArea (requestHandDrawingAreaSize drawingArea)
   return drawingArea

colorBlue = Color 0x9000 0xa700 0xca00
colorRed  = Color 0xe700 0x6d00 0x6400

playerBox :: StateIO VBox
playerBox = do
   box     <- liftIO $ vBoxNew False 0
   phase   <- phaseBox
   action  <- actionBox
   hand    <- liftIO handDrawingArea
   sep1    <- liftIO hSeparatorNew
   sep2    <- liftIO hSeparatorNew
   modify (setHand hand)
   liftIO $ boxPackStart box phase   PackNatural 0
   liftIO $ boxPackStart box sep1    PackNatural 0
   liftIO $ boxPackStart box action  PackNatural 0
   liftIO $ boxPackStart box sep2    PackNatural 0
   liftIO $ boxPackStart box hand    PackNatural 0
   return box

gamePlayBox :: StateIO VBox
gamePlayBox = do
   box       <- liftIO $ vBoxNew False 0
   tableaus  <- tableausBox
   player    <- playerBox
   sep       <- liftIO hSeparatorNew
   liftIO $ boxPackStart box tableaus PackGrow    0
   liftIO $ boxPackStart box player   PackNatural 0
   return box

menuBar :: StateIO MenuBar
menuBar = do
   menu  <- liftIO menuBarNew
   game  <- liftIO $ menuItemNewWithLabel "Game"
   modify (setMenu menu)
   liftIO $ containerAdd menu game
   return menu

mainBox :: StateIO HBox
mainBox = do
   box      <- liftIO $ hBoxNew False 0
   info     <- infoBox
   gamePlay <- gamePlayBox
   sep      <- liftIO vSeparatorNew
   liftIO $ boxPackStart box info     PackNatural 0
   liftIO $ boxPackStart box sep      PackNatural 0
   liftIO $ boxPackStart box gamePlay PackGrow    0
   return box

topLevelBox :: StateIO VBox
topLevelBox = do
   box  <- liftIO $ vBoxNew False 0
   main <- mainBox
   menu <- menuBar
   liftIO $ boxPackStart box menu PackNatural 0
   liftIO $ boxPackStart box main PackGrow    0
   return box

gameWindow :: StateIO Window
gameWindow = do
   window <- liftIO windowNew
   box    <- topLevelBox
   liftIO $ containerAdd window box
   return window

cardImageDirectory = "images" ++ pathSeparator : "cards"

loadCardPixbuf :: FilePath -> IO (String, Pixbuf)
loadCardPixbuf file = do
   pixbuf <- pixbufNewFromFile (cardImageDirectory ++ pathSeparator:file)
   return (file, pixbuf)

loadCardPixbufs :: IO (M.Map String Pixbuf)
loadCardPixbufs = do
   cardNames <- liftM (filter (\x -> x /= "." && x /= "..")) (getDirectoryContents cardImageDirectory)
   cardPairs <- mapM loadCardPixbuf cardNames
   return $ M.fromList cardPairs

main :: IO ()
main = do
   initGUI
   deck <- loadCardPixbufs
   (window, gui) <- runStateT gameWindow emptyGameGUI

   let cards = [HandCard "old_earth.jpg" False,
                HandCard "alien_uplift_center.jpg" False,
                HandCard "blaster_gem_mines.jpg" False,
                HandCard "lost_species_ark_world.jpg" False,
                HandCard "deserted_alien_world.jpg" False,
                HandCard "free_trade_association.jpg" False]
       table = [TableauCard "interstellar_casus_belli.jpg" False True]

   cardsRef <- newIORef cards
   widgetAddEvents (getHand gui) [PointerMotionMask, ButtonPressMask]
   on (getHand gui) exposeEvent (drawCurrentHand (getHand gui) deck cardsRef)
   on (getHand gui) motionNotifyEvent (motionInHand gui deck cardsRef)
   on (getHand gui) buttonPressEvent (buttonPressedInHand gui deck cardsRef)

   widgetAddEvents (getPlayerTableau gui) [PointerMotionMask]
   on (getPlayerTableau gui) exposeEvent (drawCurrentTableau (getPlayerTableau gui) deck table)
   on (getPlayerTableau gui) motionNotifyEvent (motionInTableau gui deck table)

   onDestroy window mainQuit
   widgetShowAll window
   mainGUI
