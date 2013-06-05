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
import Control.Lens
import Control.Monad
import Control.Monad.State
import GameGUI

type Deck = M.Map String Pixbuf

type Name = String
type Selected = Bool
type HasGood = Bool
data TableauCard = TableauCard Name Selected HasGood
data HandCard = HandCard Name Selected
   deriving (Eq)

type Hand = [HandCard]
type Tableau = [TableauCard]

type StateIO a = StateT GameGUI IO a

data Activity = Discard | Explore | Develop
data GUIState = GUIState {
    currentHand :: Hand
   ,exploreCards :: Hand
   ,developCards :: Hand
   ,currentActivity :: Activity
   ,numDiscard :: Int
}

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
   (opp1, _) <- liftIO $ tableauBox colorRed
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
   sep       <- liftIO hSeparatorNew
   (player, tableau) <- liftIO $ tableauBox colorBlue
   modify (setPlayerTableau tableau)
   liftIO $ boxPackStart box opponents PackGrow    0
   liftIO $ boxPackStart box sep       PackNatural 0
   liftIO $ boxPackStart box player    PackGrow    0
   return box

tableauBox :: Color -> IO (VBox, DrawingArea)
tableauBox color = do
   box          <- vBoxNew False 0
   tableau      <- tableauDrawingArea color
   playerStatus <- playerStatusBox
   boxPackStart box tableau        PackGrow    0
   boxPackStart box playerStatus   PackNatural 0
   return (box, tableau)

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

colorBoldLabel :: Label -> String -> IO Label
colorBoldLabel label color = do
   text <- labelGetText label
   labelSetMarkup label ("<b><big><span color=\"" ++ color ++ "\">" ++ text ++ "</span></big></b>")
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

doneButton = do
   button <- buttonNewWithLabel "Done"
   widgetSetSensitive button False
   return button

discardText num = "Choose " ++ show num ++ " cards to discard"
discardContextLabel num = labelNew (Just $ discardText num)

developContextLabel = labelNew (Just $ "Choose 1 card to develop")

contextBox = do
   box <- hBoxNew False 0
   context <- discardContextLabel 2
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

cardSelection :: Deck -> Pixbuf
cardSelection deck = lookupCardPixbuf deck "card_selection.png"

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
       cardHeight  = currentCardHeight width
       numCards = length cards
       ndx' = if (round y) > height `quot` 2 && ndx + 6 < numCards
              then ndx + 6
              else ndx
       card = if ndx' < numCards &&
                 ((round y) < cardHeight || ndx' >= 6 && (round y) < height `quot` 2 + cardHeight)
              then Just (cards !! ndx')
              else Nothing
   liftIO $ when (not $ isNothing card) $
      let TableauCard name _ _ = fromJust card
      in setPixbuf (getCard gui) deck name
   return True

toggleCardAtIndex :: Hand -> Int -> Hand
toggleCardAtIndex [] _        = undefined
toggleCardAtIndex ((HandCard name selected):xs) 0 = HandCard name (not selected) : xs
toggleCardAtIndex (x:xs) ndx  = x : toggleCardAtIndex xs (ndx - 1)

toggleExclusiveCardAtIndex :: Hand -> Int -> Hand
toggleExclusiveCardAtIndex [] _        = []
toggleExclusiveCardAtIndex ((HandCard name selected):xs) 0 =
   HandCard name (not selected) : toggleExclusiveCardAtIndex xs (-1)
toggleExclusiveCardAtIndex ((HandCard name _):xs) ndx =
   HandCard name False : toggleExclusiveCardAtIndex xs (ndx - 1)

getHandCardsXYForDiscard :: Hand -> Int -> [(HandCard, (Int, Int))]
getHandCardsXYForDiscard hand width =
   let xOffset = xOffsetInHand width (length hand)
       pad = cardPadding (currentCardHeight width)
       pos c@(HandCard _ selected) i = (c, (xOffset * i, if selected then 0 else pad))
   in mapI pos hand

getHandCardIndexFromXYDiscard :: Hand -> (Double, Double) -> (Int, Int) -> Maybe Int
getHandCardIndexFromXYDiscard hand pos (width, _) =
   let (cardWidth, cardHeight) = (width, currentCardHeight width)
       (x, y) = over both round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getHandCardsXYForDiscard hand width
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandDiscard gui deck stateRef = do
   let drawingArea = (getHand gui)
   state           <- liftIO $ readIORef stateRef
   (width, _)      <- liftIO $ widgetGetSize drawingArea
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   gc              <- liftIO $ gcNew drawWindow
   let hand    = currentHand state
       height  = currentCardHeight width
       cards   = getPixbufsForHand deck hand
       xOffset = xOffsetInHand width (length hand)
   liftIO$ forM_ (getHandCardsXYForDiscard hand width) (\((HandCard name _), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandDiscard gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize (getHand gui)
   pos <- eventCoordinates
   let hand = currentHand state
       ndx = getHandCardIndexFromXYDiscard hand pos size
   liftIO $ when (not . isNothing $ ndx) $
      let HandCard name _ = hand !! (fromJust ndx)
      in setPixbuf (getCard gui) deck name
   return True

buttonPressedInHandDiscard :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandDiscard gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getHand gui
       hand = currentHand state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   coords <- eventCoordinates
   let ndx = getHandCardIndexFromXYDiscard (currentHand state) coords size
   liftIO $ when ((not . isNothing) ndx &&
                  clickType == SingleClick &&
                  button == LeftButton) $ do
      let newHand = toggleCardAtIndex hand (fromJust ndx)
      writeIORef stateRef (state { currentHand = newHand })
      verifyNumberToDiscard gui newHand (numDiscard state)
      widgetQueueDraw drawingArea
   return True

getHandCardsXYForExplore :: Hand -> Hand -> Int -> ([(HandCard, (Int, Int))], [(HandCard, (Int, Int))])
getHandCardsXYForExplore hand explore width =
   let xOffset = xOffsetInHand width (1 + (length $ hand ++ explore))
       pad = cardPadding (currentCardHeight width)
       handPosition c i = (c, (xOffset * i, pad))
       explorePosition c@(HandCard _ selected) i =
         (c, (xOffset * (i + 1 + (length hand)), if selected then 0 else pad))
   in  (mapI handPosition hand, mapI explorePosition explore)

getHandCardIndexFromXYExplore :: Hand -> Hand -> (Double, Double) -> (Int, Int) -> (Maybe Int, Maybe Int)
getHandCardIndexFromXYExplore hand explore pos (width, _) =
   let (cardWidth, cardHeight) = (width, currentCardHeight width)
       (x, y) = over both round pos
       findCardFunc (((HandCard _ selected), (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       (handCards, exploreCards) = getHandCardsXYForExplore hand explore width
       zipIndices l = zip l [0..length l]
   in (foldr findCardFunc Nothing (zipIndices handCards),
       foldr findCardFunc Nothing (zipIndices exploreCards))

drawCurrentHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandExplore gui deck stateRef = do
   let drawingArea = (getHand gui)
   state      <- liftIO $ readIORef stateRef
   (width, _) <- liftIO $ widgetGetSize drawingArea
   drawWindow <- liftIO $ widgetGetDrawWindow drawingArea
   gc         <- liftIO $ gcNew drawWindow
   let height  = currentCardHeight width
   let (hand, explore) = getHandCardsXYForExplore (currentHand state) (exploreCards state) width
   liftIO $ forM_ hand (\((HandCard name _), (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ forM_ explore (\((HandCard name _), (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0)       -- dithering

   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandExplore gui deck stateRef = do
   let drawingArea = getHand gui
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize drawingArea
   coords <- eventCoordinates
   let explore = exploreCards state
       hand = currentHand state
       (handNdx, expNdx) = getHandCardIndexFromXYExplore hand explore coords size
       card = (fmap (explore !!) expNdx) `mplus` (fmap (hand !!) handNdx)
   liftIO $ when (not . isNothing $ card) $
      let (HandCard name _) = fromJust card
      in setPixbuf (getCard gui) deck name
   return True

buttonPressedInHandExplore :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandExplore gui deck stateRef = do
   let drawingArea = getHand gui
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize drawingArea
   coords <- eventCoordinates
   clickType <- eventClick
   button <- eventButton
   let explore = exploreCards state
       (handNdx, expNdx) = getHandCardIndexFromXYExplore (currentHand state) explore coords size
   liftIO $ when (clickType == SingleClick &&
                  button == LeftButton &&
                  (not . isNothing) expNdx) $ do
      let newExplore = toggleCardAtIndex explore (fromJust expNdx)
      writeIORef stateRef (state { exploreCards = newExplore })
      verifyNumberToDiscard gui newExplore (numDiscard state)
      widgetQueueDraw drawingArea
   return True

getHandCardsXYForDevelop :: Hand -> Int -> [(HandCard, (Int, Int))]
getHandCardsXYForDevelop hand width =
   let xOffset = xOffsetInHand width (length hand)
       pad = cardPadding (currentCardHeight width)
       pos c@(HandCard _ _) i = (c, (xOffset * i, pad))
   in mapI pos hand

getHandCardIndexFromXYDevelop :: Hand -> (Double, Double) -> (Int, Int) -> Maybe Int
getHandCardIndexFromXYDevelop hand pos (width, _) =
   let (cardWidth, cardHeight) = (width, currentCardHeight width)
       (x, y) = over both round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getHandCardsXYForDiscard hand width
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandDevelop gui deck stateRef = do
   let drawingArea = (getHand gui)
   state           <- liftIO $ readIORef stateRef
   (width, _)      <- liftIO $ widgetGetSize drawingArea
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   gc              <- liftIO $ gcNew drawWindow
   let hand    = currentHand state
       height  = currentCardHeight width
       cards   = getPixbufsForHand deck hand
       xOffset = xOffsetInHand width (length hand)
   select <- liftIO $ pixbufScaleSimple (cardSelection deck) (width `quot` 6) height InterpBilinear
   liftIO$ forM_ (getHandCardsXYForDevelop hand width) (\((HandCard name selected), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when selected $
         drawPixbuf drawWindow gc
                    select
                    0 0                          -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0) -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandDevelop gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   size <- liftIO $ widgetGetSize (getHand gui)
   pos <- eventCoordinates
   let hand = currentHand state
       ndx = getHandCardIndexFromXYDevelop hand pos size
   liftIO $ when (not . isNothing $ ndx) $
      let HandCard name _ = hand !! (fromJust ndx)
      in setPixbuf (getCard gui) deck name
   return True

buttonPressedInHandDevelop :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandDevelop gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getHand gui
       hand = currentHand state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   coords <- eventCoordinates
   let ndx = getHandCardIndexFromXYDiscard (currentHand state) coords size
   liftIO $ when ((not . isNothing) ndx &&
                  clickType == SingleClick &&
                  button == LeftButton) $ do
      let newHand = toggleExclusiveCardAtIndex hand (fromJust ndx)
      writeIORef stateRef (state { currentHand = newHand })
      verifyNumberToDevelop gui newHand
      widgetQueueDraw drawingArea
   return True

xOffsetInHand :: Int -> Int -> Int
xOffsetInHand width numCards = min (width `quot` 6) (width `quot` numCards)

mapI :: (a -> Int -> b) -> [a] -> [b]
mapI f l = zipWith f l [0..length l]

verifyNumberToDevelop :: GameGUI -> Hand -> IO ()
verifyNumberToDevelop gui hand = do
   let numSelected = length (filter (\(HandCard _ selected) -> selected) hand)
   widgetSetSensitive (getDone gui) (numSelected `elem` [0, 1])
   return ()

verifyNumberToDiscard :: GameGUI -> Hand -> Int -> IO ()
verifyNumberToDiscard gui hand num = do
   let numSelected = length (filter (\(HandCard _ selected) -> selected) hand)
   widgetSetSensitive (getDone gui) (numSelected == num)
   return ()


scaleCard :: Deck -> String -> Int -> Int -> IO Pixbuf
scaleCard deck name width height = pixbufScaleSimple (lookupCardPixbuf deck name) (width `quot` 6) height InterpBilinear

drawCurrentTableau :: IORef GUIState -> GameGUI -> Deck -> Tableau -> EventM EExpose Bool
drawCurrentTableau stateRef gui deck tableau = do
   let drawingArea = getPlayerTableau gui
   drawWindow      <- liftIO $ widgetGetDrawWindow drawingArea
   (width, height) <- liftIO $ widgetGetSize drawingArea
   gc              <- liftIO $ gcNew drawWindow

   let cardHeight  = currentCardHeight width
       cards   = getPixbufsForTableau deck tableau
       xOffset = width `quot` 6

   pixbufs <- liftIO $ forM cards (\card ->
         pixbufScaleSimple card (width `quot` 6) cardHeight InterpBilinear)

   cardBackPixbuf <- liftIO $
         pixbufScaleSimple (cardBack deck) (width `quot` 8) (cardHeight * 3 `quot` 4) InterpBilinear

   liftIO $ forM_ [0..length cards - 1] (\i -> do
      let TableauCard _ _ hasGood = tableau !! i
          destX = xOffset * (i `rem` 6)
          destY = if i >= 6
                  then height `quot` 2
                  else 0
      drawPixbuf drawWindow gc
                 (pixbufs !! i)                     -- pixbuf to draw
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when hasGood $ drawPixbuf drawWindow gc cardBackPixbuf 0 0
                       (destX + width `quot` 24) (destY + cardHeight `quot` 4)
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
                HandCard "blaster_gem_mines.jpg" False,
                HandCard "deserted_alien_world.jpg" False,
                HandCard "free_trade_association.jpg" False]
       explore = [HandCard "the_last_of_the_uplift_gnarssh.jpg" False,
                  HandCard "space_marines.jpg" False,
                  HandCard "space_marines.jpg" False]
       table = [TableauCard "old_earth.jpg" False True]

   stateRef <- newIORef (GUIState cards [] [] Discard 0)
   widgetAddEvents (getHand gui) [PointerMotionMask, ButtonPressMask]
   on (getHand gui) exposeEvent (drawCurrentHand gui deck stateRef)
   on (getHand gui) motionNotifyEvent (motionInHand gui deck stateRef)
   on (getHand gui) buttonPressEvent (buttonPressedInHand gui deck stateRef)

   widgetAddEvents (getPlayerTableau gui) [PointerMotionMask]
   on (getPlayerTableau gui) exposeEvent (drawCurrentTableau stateRef gui deck table)
   on (getPlayerTableau gui) motionNotifyEvent (motionInTableau gui deck table)

   onDestroy window mainQuit
   widgetShowAll window
   beginExplorePhase stateRef gui explore 2
   beginDiscard stateRef gui 2
   beginDevelopPhase stateRef gui []
   mainGUI

drawCurrentHand :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> drawCurrentHandDiscard gui deck stateRef
      Explore -> drawCurrentHandExplore gui deck stateRef
      Develop -> drawCurrentHandDevelop gui deck stateRef

motionInHand :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> motionInHandDiscard gui deck stateRef
      Explore -> motionInHandExplore gui deck stateRef
      Develop -> motionInHandDevelop gui deck stateRef

buttonPressedInHand :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> buttonPressedInHandDiscard gui deck stateRef
      Explore -> buttonPressedInHandExplore gui deck stateRef
      Develop -> buttonPressedInHandDevelop gui deck stateRef

containerRemoveChildren container = do
   toRemove <- containerGetChildren container
   mapM_ (containerRemove container) toRemove

-- Controller

beginDiscard :: IORef GUIState -> GameGUI -> Int -> IO ()
beginDiscard stateRef gui numDiscard = do
   let contextBox = getContext gui
   strikeThroughLabel (getExplore gui)
   containerRemoveChildren contextBox
   label <- discardContextLabel numDiscard
   containerAdd contextBox label
   widgetShowAll contextBox
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Discard
         ,numDiscard = numDiscard
      })

beginExplorePhase :: IORef GUIState -> GameGUI -> Hand -> Int -> IO ()
beginExplorePhase stateRef gui cards keepCount = do
   let contextBox = getContext gui
       numDiscard = (length cards) - keepCount
   colorBoldLabel (getExplore gui) "blue"
   containerRemoveChildren contextBox
   label <- discardContextLabel numDiscard
   containerAdd contextBox label
   widgetShowAll contextBox
   modifyIORef stateRef (\state ->
      state { 
          exploreCards = cards
         ,currentActivity = Explore
         ,numDiscard = numDiscard
      })

beginDevelopPhase :: IORef GUIState -> GameGUI -> Hand -> IO ()
beginDevelopPhase stateRef gui developCards = do
   let contextBox = getContext gui
   colorBoldLabel (getDevelop gui) "blue"
   containerRemoveChildren contextBox
   label <- developContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gui) True
   modifyIORef stateRef (\state ->
      state { 
          currentActivity = Develop
         ,developCards = developCards
      })
