module Main where

import Debug.Trace

import Text.Printf
import Data.IORef
import Data.Maybe
import qualified Data.Map as M

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.Events

import System.FilePath
import System.Directory (getDirectoryContents)
import Control.Monad
import Control.Monad.State as State

type Deck = M.Map String Pixbuf
type Hand = [String]

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
   ,player    :: Maybe VBox
}

type StateIO a = StateT GameGUI IO a

drawDiscardPoolText :: Int -> Int -> Int -> String
drawDiscardPoolText = printf "Draw: %d Discard: %d Pool: %d"

cardImageDims = (372, 520)
cardImage :: StateIO Image
cardImage = do
   image <- liftIO $ imageNewFromFile "images/cards/card_back.jpg"
   state <- State.get
   put $ state { card = Just image }
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
   state <- State.get
   put $ state { drawDiscardPool = Just label }
   return label

playHistoryTextView :: StateIO TextView
playHistoryTextView = do
   view <- liftIO textViewNew
   liftIO $ textViewSetEditable view False
   liftIO $ textViewSetCursorVisible view False
   state <- State.get
   put $ state { playHistory = Just view }
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

tableausBox :: StateIO VBox
tableausBox = do
   box       <- liftIO $ vBoxNew False 0
   opponents <- liftIO opponentsBox
   player    <- liftIO $ tableauBox colorBlue
   sep       <- liftIO hSeparatorNew
   state <- State.get
   put $ state {
        opponents = Just opponents
       ,player    = Just player
   }
   liftIO $ boxPackStart box opponents PackGrow    0
   liftIO $ boxPackStart box sep       PackNatural 0
   liftIO $ boxPackStart box player    PackGrow    0
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
   state <- State.get
   put $ state {
       explore = Just explore
      ,develop = Just develop
      ,settle  = Just settle
      ,consume = Just consume
      ,produce = Just produce
   }
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
   state <- State.get
   put $ state {
       done = Just done
      ,context = Just context
   }
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

setPixbuf :: Image -> Deck -> String -> IO ()
setPixbuf image deck card = imageSetFromPixbuf image (fromJust $ M.lookup card deck)

getHand :: Deck -> Hand -> [Pixbuf]
getHand deck = map (\key -> fromJust $ M.lookup key deck)

motionOccurredInHand :: GameGUI -> Deck -> Hand -> Event -> IO Bool
motionOccurredInHand gui deck cards motion = do
   let drawingArea = fromJust $ hand gui
   (width, height) <- widgetGetSize drawingArea
   let x = eventX motion
       y = eventY motion
       ndx = (round x) `quot` (xOffsetInHand width cards)
   -- TODO : check the y value too
   when (ndx < length cards && (round y) > cardPadding (currentCardHeight width)) $
      setPixbuf (fromJust $ card gui) deck (cards !! ndx)
   return True

xOffsetInHand :: Int -> Hand -> Int
xOffsetInHand width cards = min (width `quot` 6) (width `quot` length cards)

drawCurrentHand :: DrawingArea -> Deck -> Hand -> IO ()
drawCurrentHand drawingArea deck hand = do
   (width, _)      <- widgetGetSize drawingArea
   drawWindow      <- widgetGetDrawWindow drawingArea
   gc              <- gcNew drawWindow
   let height  = currentCardHeight width
       cards   = getHand deck hand
       xOffset = xOffsetInHand width hand
   pixbufs <- forM cards (\card -> pixbufScaleSimple card (width `quot` 6) height InterpBilinear)
   -- TODO: I don't really like these next two lines
   forM_ [0..length cards - 1] (\i ->
      drawPixbuf drawWindow gc
                 (pixbufs !! i)                     -- pixbuf to draw
                 0 0                                -- srcx srcy
                 (xOffset * i) (cardPadding height) -- destx desty
                 (-1) (-1) RgbDitherNone 0 0)       -- dithering
   widgetQueueDraw drawingArea

drawCurrentTableau :: DrawingArea -> Hand -> IO Bool
drawCurrentTableau drawingArea _ = do
   drawWindow      <- widgetGetDrawWindow drawingArea
   (width, height) <- widgetGetSize drawingArea
   gc              <- gcNew drawWindow
   --drawPixbuf drawWindow gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
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
   state <- State.get
   put $ state { hand = Just hand }
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
   state <- State.get
   put $ state { menu = Just menu }
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

onExpose_ :: WidgetClass w => w -> IO () -> IO ()
onExpose_ widget func = do
   onExpose widget exposeFunc
   return ()
   where exposeFunc :: Event -> IO Bool
         exposeFunc _ = do
            func
            return True

main :: IO ()
main = do
   initGUI
   deck <- loadCardPixbufs
   (window, gui) <- runStateT gameWindow emptyGameGUI
   let cards = ["old_earth.jpg", "alien_uplift_center.jpg", "blaster_gem_mines.jpg"]
   onMotionNotify (fromJust $ hand gui) True (motionOccurredInHand gui deck cards)
   onExpose_ (fromJust $ hand gui) (drawCurrentHand (fromJust $ hand gui) deck cards)
   onDestroy window mainQuit
   widgetShowAll window
   mainGUI
