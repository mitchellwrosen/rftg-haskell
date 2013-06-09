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
data Selected = Disabled | Selected | UnSelected
   deriving (Eq, Show)
type HasGood = Bool
data TableauCard = TableauCard Name Selected HasGood
   deriving (Show)
data HandCard = HandCard Name Selected

instance Eq TableauCard where
   (TableauCard a1 _ a2) == (TableauCard b1 _ b2) = a1 == b1 && a2 == b2

instance Eq HandCard where
   (HandCard a _) == (HandCard b _) = a == b

type Hand = [HandCard]
type Tableau = [TableauCard]

type StateIO a = StateT GameGUI IO a

data Activity = Discard | Explore | Develop | Settle | ChooseConsumePower | ChooseGood | Produce
data GUIState = GUIState {
    currentHand :: Hand
   ,exploreCards :: Hand
   ,currentTableau :: Tableau
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
settleContextLabel = labelNew (Just $ "Choose 1 card to settle")
chooseConsumePowerContextLabel = labelNew (Just $ "Choose a consume power")
chooseGoodContextLabel cardName = labelNew (Just $ "Choose a good for " ++ cardName)
produceContextLabel = labelNew (Just $ "Choose a windfall to produce on")

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

cardDisabled :: Deck -> Pixbuf
cardDisabled deck = lookupCardPixbuf deck "card_disabled.png"

setPixbuf :: Image -> Deck -> String -> IO ()
setPixbuf image deck card = imageSetFromPixbuf image (lookupCardPixbuf deck card)

getPixbufsForHand :: Deck -> Hand -> [Pixbuf]
getPixbufsForHand deck = map (\(HandCard name _) -> lookupCardPixbuf deck name)

getPixbufsForTableau :: Deck -> Tableau -> [Pixbuf]
getPixbufsForTableau deck = map (\(TableauCard name _ _) -> lookupCardPixbuf deck name)

getTableauCardsXY :: Tableau -> (Int, Int) -> [(TableauCard, (Int, Int))]
getTableauCardsXY tableau (width, height) =
   let xOffset = width `quot` 6
       pos card i = (card, (xOffset * (i `rem` 6), if i >= 6 then height `quot` 2 else 0))
   in mapI pos tableau

cardDims :: (Int, Int) -> (Int, Int)
cardDims (width, _) = (width `quot` 6, currentCardHeight width)

getTableauCardIndexFromXY :: Tableau -> (Double, Double) -> (Int, Int) -> Maybe Int
getTableauCardIndexFromXY tableau pos size =
   let (cardWidth, cardHeight) = cardDims size
       (x, y) = over both round pos
       findCardFunc ((_, (cardX, cardY)), ndx) Nothing
          | cardX < x && x < cardX + cardWidth &&
            cardY < y && y < cardY + cardHeight = Just ndx
          | otherwise = Nothing
       findCardFunc _ foundCard = foundCard
       cards = getTableauCardsXY tableau size
       zipIndices l = zip l [0..length l]
   in foldr findCardFunc Nothing (zipIndices cards)

drawCurrentTableau :: IORef GUIState -> GameGUI -> Deck -> EventM EExpose Bool
drawCurrentTableau stateRef gui deck = do
   let drawingArea = getPlayerTableau gui
   state <- liftIO $ readIORef stateRef
   let tableau = currentTableau state
   drawWindow <- liftIO $ widgetGetDrawWindow drawingArea
   size@(width, height) <- liftIO $ widgetGetSize drawingArea
   gc <- liftIO $ gcNew drawWindow
   let cardHeight  = currentCardHeight width
       cards = getTableauCardsXY tableau size
   cardBackPixbuf <- liftIO $
         pixbufScaleSimple (cardBack deck) (width `quot` 8) (cardHeight * 3 `quot` 4) InterpBilinear
   selectPixbuf <- liftIO $ scaleCard deck "card_selection.png" width cardHeight
   disabledPixbuf <- liftIO $ scaleCard deck "card_disabled.png" width cardHeight
   liftIO $ forM_ cards (\((TableauCard name selected hasGood), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width cardHeight
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when hasGood $
         let goodX = destX + width `quot` 24
             goodY = destY + cardHeight `quot` 4
         in drawPixbuf drawWindow gc cardBackPixbuf 0 0
                       goodX goodY
                       (-1) (-1) RgbDitherNone 0 0
      when (isSelected selected) $
         drawPixbuf drawWindow gc
                    selectPixbuf
                    0 0                                -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isDisabled selected) $
         drawPixbuf drawWindow gc
                    disabledPixbuf
                    0 0                                -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0)       -- dithering
   liftIO $ widgetQueueDraw drawingArea
   return True

motionInTableau :: GameGUI -> Deck -> Tableau -> EventM EMotion Bool
motionInTableau gui deck tableau = do
   let drawingArea = getPlayerTableau gui
   size <- liftIO $ widgetGetSize drawingArea
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   liftIO $ when (not $ isNothing ndx) $
      let TableauCard name _ _ = tableau !! fromJust ndx
      in setPixbuf (getCard gui) deck name
   return True

verifyNumberToConsume :: GameGUI -> Tableau -> Int -> IO ()
verifyNumberToConsume gui tableau num =
   let numSelected = length (filter (\(TableauCard _ selected _) -> isSelected selected) tableau)
   in widgetSetSensitive (getDone gui) (numSelected == num)

buttonPressedInTableauChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInTableauChooseGood gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getPlayerTableau gui
       tableau = currentTableau state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   liftIO $ when ((not . isNothing) ndx &&
                  clickType == SingleClick &&
                  button == LeftButton) $ do
      let newTableau = toggleTableauCardAtIndex tableau (fromJust ndx)
      writeIORef stateRef (state { currentTableau = newTableau })
      verifyNumberToConsume gui newTableau (numDiscard state)
      widgetQueueDraw drawingArea
   return True

buttonPressedInTableauProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInTableauProduce gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   let drawingArea = getPlayerTableau gui
       tableau = currentTableau state
   size <- liftIO $ widgetGetSize drawingArea
   clickType <- eventClick
   button <- eventButton
   pos <- eventCoordinates
   let ndx = getTableauCardIndexFromXY tableau pos size
   liftIO $ when ((not . isNothing) ndx &&
                  clickType == SingleClick &&
                  button == LeftButton) $ do
      let newTableau = toggleTableauCardAtIndex tableau (fromJust ndx)
      writeIORef stateRef (state { currentTableau = newTableau })
      verifyNumberToConsume gui newTableau (numDiscard state)
      widgetQueueDraw drawingArea
   return True

isSelected :: Selected -> Bool
isSelected Selected = True
isSelected _ = False

isDisabled :: Selected -> Bool
isDisabled Disabled = True
isDisabled _ = False

toggleSelection :: Selected -> Selected
toggleSelection Selected = UnSelected
toggleSelection UnSelected = Selected
toggleSelection a = a

toggleTableauCardAtIndex :: Tableau -> Int -> Tableau
toggleTableauCardAtIndex [] _        = []
toggleTableauCardAtIndex ((TableauCard name selected hasGood):xs) 0 =
   TableauCard name (toggleSelection selected) hasGood : xs
toggleTableauCardAtIndex (x:xs) ndx  =
   x : toggleTableauCardAtIndex xs (ndx - 1)

toggleHandCardAtIndex :: Hand -> Int -> Hand
toggleHandCardAtIndex [] _        = []
toggleHandCardAtIndex ((HandCard name selected):xs) 0 = HandCard name (toggleSelection selected) : xs
toggleHandCardAtIndex (x:xs) ndx  = x : toggleHandCardAtIndex xs (ndx - 1)

toggleExclusiveCardAtIndex :: Hand -> Int -> Hand
toggleExclusiveCardAtIndex [] _        = []
toggleExclusiveCardAtIndex ((HandCard name selected):xs) 0 =
   HandCard name (toggleSelection selected) : toggleExclusiveCardAtIndex xs (-1)
toggleExclusiveCardAtIndex ((HandCard name Selected):xs) ndx =
   HandCard name UnSelected : toggleExclusiveCardAtIndex xs (ndx - 1)
toggleExclusiveCardAtIndex ((HandCard name selected):xs) ndx =
   HandCard name selected : toggleExclusiveCardAtIndex xs (ndx - 1)

getHandCardsXYForDiscard :: Hand -> Int -> [(HandCard, (Int, Int))]
getHandCardsXYForDiscard hand width =
   let xOffset = xOffsetInHand width (length hand)
       pad = cardPadding (currentCardHeight width)
       pos c@(HandCard _ selected) i = (c, (xOffset * i, if isSelected selected then 0 else pad))
   in mapI pos hand

getHandCardIndexFromXYDiscard :: Hand -> (Double, Double) -> (Int, Int) -> Maybe Int
getHandCardIndexFromXYDiscard hand pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
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
   discardedPixbuf <- liftIO $ scaleCard deck "card_discarded.png" width height
   liftIO$ forM_ (getHandCardsXYForDiscard hand width) (\((HandCard name selected), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isSelected selected) $ drawPixbuf drawWindow gc
                                 discardedPixbuf
                                 0 0
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
      let newHand = toggleHandCardAtIndex hand (fromJust ndx)
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
         (c, (xOffset * (i + 1 + (length hand)), if isSelected selected then 0 else pad))
   in  (mapI handPosition hand, mapI explorePosition explore)

getHandCardIndexFromXYExplore :: Hand -> Hand -> (Double, Double) -> (Int, Int) -> (Maybe Int, Maybe Int)
getHandCardIndexFromXYExplore hand explore pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
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
   disabledPixbuf <- liftIO $ scaleCard deck "card_disabled.png" width height
   liftIO $ forM_ hand (\((HandCard name _), (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0        -- dithering
        drawPixbuf drawWindow gc
                   disabledPixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0)       -- dithering

   discardedPixbuf <- liftIO $ scaleCard deck "card_discarded.png" width height
   liftIO $ forM_ explore (\((HandCard name selected), (destX, destY)) -> do
        pixbuf <- scaleCard deck name width height
        drawPixbuf drawWindow gc
                   pixbuf
                   0 0                                -- srcx srcy
                   destX destY
                   (-1) (-1) RgbDitherNone 0 0        -- dithering
        when (isSelected selected) $ 
           drawPixbuf drawWindow gc
                      discardedPixbuf
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
      let newExplore = toggleHandCardAtIndex explore (fromJust expNdx)
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
getHandCardIndexFromXYDevelop hand pos size@(width, _) =
   let (cardWidth, cardHeight) = cardDims size
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
   selectPixbuf <- liftIO $ scaleCard deck "card_selection.png" width height
   disablePixbuf <- liftIO $ scaleCard deck "card_disabled.png" width height
   liftIO$ forM_ (getHandCardsXYForDevelop hand width) (\((HandCard name selected), (destX, destY)) -> do
      pixbuf <- scaleCard deck name width height
      drawPixbuf drawWindow gc
                 pixbuf
                 0 0                                -- srcx srcy
                 destX destY
                 (-1) (-1) RgbDitherNone 0 0        -- dithering
      when (isSelected selected) $
         drawPixbuf drawWindow gc
                    selectPixbuf
                    0 0                          -- srcx srcy
                    destX destY
                    (-1) (-1) RgbDitherNone 0 0  -- dithering
      when (isDisabled selected) $
         drawPixbuf drawWindow gc
                    disablePixbuf
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

drawCurrentHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandSettle = drawCurrentHandDevelop

motionInHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandSettle = motionInHandDevelop

buttonPressedInHandSettle :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandSettle = buttonPressedInHandDevelop

drawCurrentHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandChooseConsume = drawCurrentHandDevelop

motionInHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandChooseConsume = motionInHandDevelop

buttonPressedInHandChooseConsume :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandChooseConsume _ _ _ = return True

drawCurrentHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandChooseGood = drawCurrentHandDevelop

drawCurrentHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHandProduce = drawCurrentHandDevelop

motionInHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandChooseGood = motionInHandDevelop

motionInHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHandProduce = motionInHandDevelop

buttonPressedInHandChooseGood :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandChooseGood _ _ _ = return True

buttonPressedInHandProduce :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHandProduce _ _ _ = return True

xOffsetInHand :: Int -> Int -> Int
xOffsetInHand width numCards = min (width `quot` 6) (width `quot` numCards)

mapI :: (a -> Int -> b) -> [a] -> [b]
mapI f l = zipWith f l [0..length l]

verifyNumberToDevelop :: GameGUI -> Hand -> IO ()
verifyNumberToDevelop gui hand = do
   let numSelected = length (filter (\(HandCard _ selected) -> isSelected selected) hand)
   widgetSetSensitive (getDone gui) (numSelected `elem` [0, 1])
   return ()

verifyNumberToDiscard :: GameGUI -> Hand -> Int -> IO ()
verifyNumberToDiscard gui hand num = do
   let numSelected = length (filter (\(HandCard _ selected) -> isSelected selected) hand)
   widgetSetSensitive (getDone gui) (numSelected == num)
   return ()


scaleCard :: Deck -> String -> Int -> Int -> IO Pixbuf
scaleCard deck name width height = pixbufScaleSimple (lookupCardPixbuf deck name) (width `quot` 6) height InterpBilinear


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

consumePowerOptionMenu :: [String] -> IO ComboBox
consumePowerOptionMenu powers = do
   menu <- comboBoxNewText
   mapM_ (comboBoxAppendText menu) powers
   return menu

main :: IO ()
main = do
   initGUI
   deck <- loadCardPixbufs
   (window, gui) <- runStateT gameWindow emptyGameGUI

   let cards = [HandCard "old_earth.jpg" UnSelected,
                HandCard "alien_uplift_center.jpg" UnSelected,
                HandCard "blaster_gem_mines.jpg" UnSelected,
                HandCard "blaster_gem_mines.jpg" UnSelected,
                HandCard "deserted_alien_world.jpg" UnSelected,
                HandCard "free_trade_association.jpg" UnSelected]
       explore = [HandCard "the_last_of_the_uplift_gnarssh.jpg" UnSelected,
                  HandCard "space_marines.jpg" UnSelected,
                  HandCard "space_marines.jpg" UnSelected]
       table = [TableauCard "old_earth.jpg" UnSelected True
                ,TableauCard "alien_uplift_center.jpg" UnSelected True
                ,TableauCard "alien_uplift_center.jpg" UnSelected False]

   stateRef <- newIORef (GUIState cards [] table Discard 0)
   widgetAddEvents (getHand gui) [PointerMotionMask, ButtonPressMask]
   on (getHand gui) exposeEvent (drawCurrentHand gui deck stateRef)
   on (getHand gui) motionNotifyEvent (motionInHand gui deck stateRef)
   on (getHand gui) buttonPressEvent (buttonPressedInHand gui deck stateRef)

   widgetAddEvents (getPlayerTableau gui) [PointerMotionMask]
   on (getPlayerTableau gui) exposeEvent (drawCurrentTableau stateRef gui deck)
   on (getPlayerTableau gui) motionNotifyEvent (motionInTableau gui deck table)
   on (getPlayerTableau gui) buttonPressEvent (buttonPressedInTableau gui deck stateRef)

   onDestroy window mainQuit
   widgetShowAll window
   beginDevelopPhase stateRef gui [
                HandCard "alien_uplift_center.jpg" UnSelected,
                HandCard "blaster_gem_mines.jpg" UnSelected]
   beginSettlePhase stateRef gui [
                HandCard "deserted_alien_world.jpg" UnSelected,
                HandCard "old_earth.jpg" UnSelected]
   beginChooseConsumePower stateRef gui []
   beginDiscard stateRef gui 2
   beginExplorePhase stateRef gui explore 2
   beginChooseGood stateRef gui [TableauCard "old_earth.jpg" UnSelected True] 1 "Old Earth"
   beginProducePhase stateRef gui [TableauCard "alien_uplift_center.jpg" UnSelected False] 1
   mainGUI

buttonPressedInTableau :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInTableau gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      ChooseGood -> buttonPressedInTableauChooseGood gui deck stateRef
      Produce -> buttonPressedInTableauProduce gui deck stateRef
      _ -> return True

drawCurrentHand :: GameGUI -> Deck -> IORef GUIState -> EventM EExpose Bool
drawCurrentHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> drawCurrentHandDiscard gui deck stateRef
      Explore -> drawCurrentHandExplore gui deck stateRef
      Develop -> drawCurrentHandDevelop gui deck stateRef
      Settle -> drawCurrentHandSettle gui deck stateRef
      ChooseConsumePower -> drawCurrentHandChooseConsume gui deck stateRef
      ChooseGood -> drawCurrentHandChooseGood gui deck stateRef
      Produce -> drawCurrentHandProduce gui deck stateRef

motionInHand :: GameGUI -> Deck -> IORef GUIState -> EventM EMotion Bool
motionInHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> motionInHandDiscard gui deck stateRef
      Explore -> motionInHandExplore gui deck stateRef
      Develop -> motionInHandDevelop gui deck stateRef
      Settle  -> motionInHandSettle gui deck stateRef
      ChooseConsumePower -> motionInHandChooseConsume gui deck stateRef
      ChooseGood -> motionInHandChooseGood gui deck stateRef
      Produce -> motionInHandProduce gui deck stateRef

buttonPressedInHand :: GameGUI -> Deck -> IORef GUIState -> EventM EButton Bool
buttonPressedInHand gui deck stateRef = do
   state <- liftIO $ readIORef stateRef
   case currentActivity state of
      Discard -> buttonPressedInHandDiscard gui deck stateRef
      Explore -> buttonPressedInHandExplore gui deck stateRef
      Develop -> buttonPressedInHandDevelop gui deck stateRef
      Settle -> buttonPressedInHandSettle gui deck stateRef
      ChooseConsumePower -> buttonPressedInHandChooseConsume gui deck stateRef
      ChooseGood -> buttonPressedInHandChooseGood gui deck stateRef
      Produce -> buttonPressedInHandProduce gui deck stateRef

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

enableAllCards :: Hand -> Hand
enableAllCards = map enableCard
   where enableCard (HandCard name _) = HandCard name UnSelected

enableAndDisableCards :: Hand -> Hand -> Hand
enableAndDisableCards hand enableCards = map enableOrDisableCard hand
   where enableOrDisableCard card@(HandCard name selected) =
            if card `elem` enableCards
            then HandCard name UnSelected
            else HandCard name Disabled

enableAndDisableTableauCards :: Tableau -> Tableau -> Tableau
enableAndDisableTableauCards table enableCards = map enableOrDisableCard table
   where enableOrDisableCard card@(TableauCard name selected hasGood) =
            if card `elem` enableCards
            then TableauCard name UnSelected hasGood
            else TableauCard name Disabled hasGood

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
         ,currentHand = (enableAndDisableCards (currentHand state) developCards)
      })

beginSettlePhase :: IORef GUIState -> GameGUI -> Hand -> IO ()
beginSettlePhase stateRef gui settleCards = do
   let contextBox = getContext gui
   colorBoldLabel (getSettle gui) "blue"
   containerRemoveChildren contextBox
   label <- settleContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gui) True
   modifyIORef stateRef (\state ->
      state { 
          currentActivity = Settle
         ,currentHand = (enableAndDisableCards (currentHand state) settleCards)
      })

beginProducePhase :: IORef GUIState -> GameGUI -> Tableau -> Int -> IO ()
beginProducePhase stateRef gui eligibleCards numGoods = do
   let contextBox = getContext gui
   colorBoldLabel (getProduce gui) "blue"
   containerRemoveChildren contextBox
   label <- produceContextLabel
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gui) False
   modifyIORef stateRef (\state ->
      state {
          currentActivity = Produce
         ,currentTableau = (enableAndDisableTableauCards (currentTableau state) eligibleCards)
      })

beginChooseConsumePower :: IORef GUIState -> GameGUI -> [String] -> IO ()
beginChooseConsumePower stateRef gui consumePowers = do
   let contextBox = getContext gui
   colorBoldLabel (getConsume gui) "blue"
   containerRemoveChildren contextBox
   label <- chooseConsumePowerContextLabel
   menu <- consumePowerOptionMenu consumePowers
   containerAdd contextBox label
   boxPackStart contextBox menu PackNatural 0
   widgetShowAll contextBox
   widgetSetSensitive (getDone gui) True
   modifyIORef stateRef (\state ->
      state {
          currentActivity = ChooseConsumePower
      })

beginChooseGood :: IORef GUIState -> GameGUI -> Tableau -> Int -> String -> IO ()
beginChooseGood stateRef gui eligibleCards numGoods cardName = do
   let contextBox = getContext gui
   containerRemoveChildren contextBox
   label <- chooseGoodContextLabel cardName
   containerAdd contextBox label
   widgetShowAll contextBox
   widgetSetSensitive (getDone gui) False
   modifyIORef stateRef (\state ->
      state { 
          currentActivity = ChooseGood
         ,numDiscard = numGoods
         ,currentTableau = (enableAndDisableTableauCards (currentTableau state) eligibleCards)
      })
